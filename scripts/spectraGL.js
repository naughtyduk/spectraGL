/*
 * spectraGL – WebGL Interactive Effects Library
 * -----------------------------------------------------------------------------
 *
 * Author: NaughtyDuk© – https://spectragl.naughtyduk.com
 * Licence: Dual Licence
 *
 * Modes: Aurora, Shimmer, Pixel, Komorebi, Nebula, Floral, Oil, Diva
 */

(function () {
  "use strict";

  /* --------------------------------------------------
   *  WebGL Context Detection
   * ------------------------------------------------*/
  const WebGLSupport = {
    webgl2: false,
    webgl1: false,
    detected: false,

    detect() {
      if (this.detected) return;

      const canvas = document.createElement("canvas");

      try {
        const gl2 = canvas.getContext("webgl2");
        if (gl2) {
          this.webgl2 = true;
          this.webgl1 = true;
        }
      } catch (e) {}

      if (!this.webgl2) {
        try {
          const gl1 =
            canvas.getContext("webgl") ||
            canvas.getContext("experimental-webgl");
          if (gl1) {
            this.webgl1 = true;
          }
        } catch (e) {}
      }

      this.detected = true;
    },

    getSupported() {
      this.detect();
      if (this.webgl2) return "webgl2";
      if (this.webgl1) return "webgl";
      return false;
    },
  };

  /* --------------------------------------------------
   *  Global State Management
   * ------------------------------------------------*/
  let globalRenderer = null;
  let instanceCounter = 0;
  let activeInstances = new Map();
  let animationFrameId = null;
  let isAnimating = false;
  let resizeObserver = null;
  let resizeDebounceTimer = null;

  let lastFrameTime = 0;
  let targetFrameTime = 1000 / 60;
  let frameSkipCounter = 0;
  let isPageVisible = true;

  let helperGUIs = [];
  let lilGuiLoaded = false;
  let lilGuiLoadPromise = null;

  function isSafari() {
    const ua = navigator.userAgent;
    return /^((?!chrome|android).)*safari/i.test(ua);
  }

  function isLowEndDevice() {
    const isMobile =
      /Android|webOS|iPhone|iPad|iPod|BlackBerry|IEMobile|Opera Mini/i.test(
        navigator.userAgent
      );
    const cores = navigator.hardwareConcurrency || 2;
    const memory = navigator.deviceMemory || 4;
    return isMobile || cores <= 2 || memory <= 2 || isSafari();
  }

  /* --------------------------------------------------
   *  Noise Functions (GLSL)
   * ------------------------------------------------*/
  const NOISE_GLSL = `
    vec3 mod289(vec3 x) { return x - floor(x * (1.0 / 289.0)) * 289.0; }
    vec2 mod289(vec2 x) { return x - floor(x * (1.0 / 289.0)) * 289.0; }
    vec3 permute(vec3 x) { return mod289(((x*34.0)+1.0)*x); }

    float snoise(vec2 v) {
      const vec4 C = vec4(0.211324865405187, 0.366025403784439,
                         -0.577350269189626, 0.024390243902439);
      vec2 i  = floor(v + dot(v, C.yy));
      vec2 x0 = v - i + dot(i, C.xx);
      vec2 i1 = (x0.x > x0.y) ? vec2(1.0, 0.0) : vec2(0.0, 1.0);
      vec4 x12 = x0.xyxy + C.xxzz;
      x12.xy -= i1;
      i = mod289(i);
      vec3 p = permute(permute(i.y + vec3(0.0, i1.y, 1.0)) + i.x + vec3(0.0, i1.x, 1.0));
      vec3 m = max(0.5 - vec3(dot(x0,x0), dot(x12.xy,x12.xy), dot(x12.zw,x12.zw)), 0.0);
      m = m*m; m = m*m;
      vec3 x = 2.0 * fract(p * C.www) - 1.0;
      vec3 h = abs(x) - 0.5;
      vec3 ox = floor(x + 0.5);
      vec3 a0 = x - ox;
      m *= 1.79284291400159 - 0.85373472095314 * (a0*a0 + h*h);
      vec3 g;
      g.x = a0.x * x0.x + h.x * x0.y;
      g.yz = a0.yz * x12.xz + h.yz * x12.yw;
      return 130.0 * dot(m, g);
    }

    float fbm(vec2 p, int octaves) {
      float value = 0.0;
      float amplitude = 0.5;
      float frequency = 1.0;
      for (int i = 0; i < 6; i++) {
        if (i >= octaves) break;
        value += amplitude * snoise(p * frequency);
        frequency *= 2.0;
        amplitude *= 0.5;
      }
      return value;
    }
  `;

  /* --------------------------------------------------
   *  Common Vertex Shader (with displacement)
   * ------------------------------------------------*/
  const VERTEX_SHADER = `
    uniform float uTime;
    uniform vec2 uMouse;
    uniform float uFoldIntensity;
    uniform float uFoldScale;
    uniform float uFoldSpeed;
    uniform float uReactiveStrength;
    uniform float uMouseRadius;
    uniform vec2 uResolution;

    varying vec2 vUv;
    varying vec3 vNormal;
    varying vec3 vPosition;
    varying float vDisplacement;

    ${NOISE_GLSL}

    void main() {
      vUv = uv;

      vec3 pos = position;

      float t = uTime * uFoldSpeed;

      float noise1 = snoise(uv * uFoldScale * 2.0 + t * 0.3) * 0.5;
      float noise2 = snoise(uv * uFoldScale * 4.0 - t * 0.2) * 0.25;
      float noise3 = snoise(uv * uFoldScale * 8.0 + t * 0.1) * 0.125;

      float rawDisp = (noise1 + noise2 + noise3) * uFoldIntensity;
      float displacement = rawDisp * 0.00015;

      vec2 mousePos = uMouse;
      float mouseDist = distance(uv, mousePos);
      float mouseInfluence = 1.0 - smoothstep(0.0, uMouseRadius, mouseDist);
      float rawMouseDisp = mouseInfluence * uReactiveStrength * 0.5;
      displacement += rawMouseDisp * 0.5;

      pos.z += displacement;

      vDisplacement = displacement;
      vPosition = pos;

      float eps = 0.01;
      vec2 uvPlusX = uv + vec2(eps, 0.0);
      vec2 uvPlusY = uv + vec2(0.0, eps);
      
      float noise1x = snoise(uvPlusX * uFoldScale * 2.0 + t * 0.3) * 0.5;
      float noise2x = snoise(uvPlusX * uFoldScale * 4.0 - t * 0.2) * 0.25;
      float noise3x = snoise(uvPlusX * uFoldScale * 8.0 + t * 0.1) * 0.125;
      
      float noise1y = snoise(uvPlusY * uFoldScale * 2.0 + t * 0.3) * 0.5;
      float noise2y = snoise(uvPlusY * uFoldScale * 4.0 - t * 0.2) * 0.25;
      float noise3y = snoise(uvPlusY * uFoldScale * 8.0 + t * 0.1) * 0.125;
      
      float dx = ((noise1x + noise2x + noise3x) - (noise1 + noise2 + noise3)) * uFoldIntensity;
      float dy = ((noise1y + noise2y + noise3y) - (noise1 + noise2 + noise3)) * uFoldIntensity;

      vec3 tangent = normalize(vec3(eps, 0.0, dx));
      vec3 bitangent = normalize(vec3(0.0, eps, dy));
      vNormal = normalize(cross(tangent, bitangent));

      gl_Position = projectionMatrix * modelViewMatrix * vec4(pos, 1.0);
    }
  `;

  /* --------------------------------------------------
   *  Mode-Specific Fragment Shaders
   * ------------------------------------------------*/

  // AURORA - Light through prism / caustics effect
  const AURORA_FRAGMENT = `
    uniform float uTime;
    uniform vec2 uMouse;
    uniform vec2 uMouseVelocity;
    uniform vec2 uDisplacement;
    uniform float uPulse;
    uniform vec2 uResolution;
    uniform vec3 uColors[7];
    uniform float uColorBlend;
    uniform float uColorBalance[7];
    uniform float uDirection;
    uniform float uSpeed;
    uniform float uRimLight;
    uniform float uRimIntensity;
    uniform vec3 uRimColor;
    uniform float uRimFalloff;
    uniform float uGrain;
    uniform float uFoldIntensity;
    uniform float uFoldScale;
    uniform float uScrollOffset;
    uniform float uReactiveStrength;
    uniform float uMouseRadius;
    uniform float uClickOffset;
    uniform vec2 uLastClickPos;
    uniform sampler2D uTextMask;
    uniform float uUseTextMask;

    varying vec2 vUv;
    varying vec3 vNormal;
    varying vec3 vPosition;
    varying float vDisplacement;

    ${NOISE_GLSL}

    float caustic(vec2 uv, float t, vec2 dir) {
      float c = 0.0;
      c += sin(uv.x * 2.0 + t * 0.4 * dir.x) * sin(uv.y * 1.8 - t * 0.3 * dir.y);
      c += sin(uv.x * 1.5 - t * 0.25 * dir.x + uv.y * 2.2) * 0.6;
      c += sin((uv.x * dir.x + uv.y * dir.y) * 1.8 + t * 0.35) * 0.4;
      
      return c * 0.3 + 0.5;
    }
    
    vec3 rainbowSpectrum(float phase, vec3 colors[7], float balance[7], float blendSharpness) {
      float index = phase * 7.0;
      float blend = fract(index);
      int i1 = int(floor(index)) % 7;
      int i2 = (i1 + 1) % 7;
      
      if (blendSharpness < 0.1) {
        blend = blend < 0.5 ? 0.0 : 1.0;
      } else if (blendSharpness < 0.25) {
        blend = smoothstep(0.3, 0.7, blend);
      } else {
        blend = blend * blend * (3.0 - 2.0 * blend);
      }
      
      return mix(
        colors[i1] * balance[i1],
        colors[i2] * balance[i2],
        blend
      );
    }

    void main() {
      vec2 uv = vUv;
      float aspect = uResolution.x / uResolution.y;
      
      float t = uTime * uSpeed * 0.15;
      
      vec2 dirOffset = vec2(1.0, 1.0);
      if (uDirection == 1.0) dirOffset = vec2(0.0, 1.0);
      else if (uDirection == 2.0) dirOffset = vec2(0.0, -1.0);
      else if (uDirection == 3.0) dirOffset = vec2(-1.0, 0.0);
      else if (uDirection == 4.0) dirOffset = vec2(1.0, 0.0);

      vec2 mousePos = uMouse;
      float mouseDist = distance(uv, mousePos);
      float mouseInfluence = smoothstep(uMouseRadius * 2.0, 0.0, mouseDist) * uReactiveStrength;
      
      vec2 displaceOffset = uDisplacement * 0.15;
      
      float scrollY = uScrollOffset;

      float blendSharpness = uColorBlend == 1.0 ? 0.15 : (uColorBlend == 2.0 ? 0.05 : 0.35);

      // ---------------------------------------------
      // Create flowing caustic light patterns
      // ---------------------------------------------
      
      float clickInfluence = smoothstep(0.9, 0.0, distance(uv, uLastClickPos));
      float tWithOffset = t + uClickOffset * clickInfluence * 1.5;
      
      vec2 causticUV1 = uv * 1.5 * uFoldScale + vec2(tWithOffset * 0.3 * dirOffset.x, tWithOffset * 0.2 * dirOffset.y + scrollY * 0.3) + displaceOffset;
      vec2 causticUV2 = uv * 2.2 * uFoldScale + vec2(-tWithOffset * 0.25 * dirOffset.x, tWithOffset * 0.35 * dirOffset.y + scrollY * 0.5) + displaceOffset * 0.7;
      vec2 causticUV3 = uv * 0.8 * uFoldScale + vec2(tWithOffset * 0.15 * dirOffset.x, -tWithOffset * 0.2 * dirOffset.y + scrollY * 0.2) + displaceOffset * 1.2;
      
      float c1 = caustic(causticUV1, tWithOffset * 2.0, dirOffset);
      float c2 = caustic(causticUV2 + vec2(50.0), tWithOffset * 1.5, dirOffset);
      float c3 = caustic(causticUV3 + vec2(100.0), tWithOffset * 2.5, dirOffset);
      
      float noiseDistort = snoise(uv * 2.0 * uFoldScale + tWithOffset * 0.2) * 0.2;
      
      // ---------------------------------------------
      // Aurora color dispersion
      // ---------------------------------------------
      
      float phase1 = fract(c1 + noiseDistort + mouseInfluence * 0.5);
      float phase2 = fract(c2 + noiseDistort * 0.8 - mouseInfluence * 0.3);
      float phase3 = fract(c3 + noiseDistort * 0.6);
      
      vec3 color = uColors[0] * uColorBalance[0];
      
      float bgLuminance = dot(color, vec3(0.299, 0.587, 0.114));
      bool isLightBg = bgLuminance > 0.5;
      
      vec3 spectrum[7];
      float spectrumBalance[7];
      for (int i = 0; i < 7; i++) {
        spectrum[i] = uColors[i];
        spectrumBalance[i] = uColorBalance[i];
      }
      
      vec3 rainbow1 = rainbowSpectrum(phase1, spectrum, spectrumBalance, blendSharpness);
      float intensity1 = c1 * uFoldIntensity;
      
      if (isLightBg) {
        color = mix(color, rainbow1, intensity1 * 0.65);
      } else {
        color = mix(color, color + rainbow1 * 0.7, intensity1 * 0.6);
      }
      
      vec3 rainbow2 = rainbowSpectrum(phase2, spectrum, spectrumBalance, blendSharpness);
      float intensity2 = c2 * uFoldIntensity * 0.7;
      
      if (isLightBg) {
        color = mix(color, rainbow2, intensity2 * 0.45);
      } else {
        color = mix(color, color + rainbow2 * 0.6, intensity2 * 0.4);
      }
      
      vec3 rainbow3 = rainbowSpectrum(phase3, spectrum, spectrumBalance, blendSharpness);
      float intensity3 = c3 * uFoldIntensity * 0.5;
      
      if (isLightBg) {
        color = mix(color, rainbow3, intensity3 * 0.3);
      } else {
        color = mix(color, color + rainbow3 * 0.5, intensity3 * 0.25);
      }
      
      float softAurora = (c1 * c2 * c3) * uFoldIntensity;
      vec3 auroraColor = mix(rainbow1, rainbow2, 0.5);
      color += auroraColor * softAurora * 0.2;

      // ---------------------------------------------
      // Bright hotspots where light converges
      // ---------------------------------------------
      if (uRimLight > 0.5) {
        float convergence = c1 * c2 * c3;
        float hotspot = smoothstep(0.15, 0.35, convergence) * uRimIntensity;
        color += uRimColor * hotspot * 0.8;
        
        float mouseAurora = smoothstep(uMouseRadius, 0.0, mouseDist) * uReactiveStrength * 0.5;
        color += uRimColor * mouseAurora;
      }

      // ---------------------------------------------
      // Glassy depth effect
      // ---------------------------------------------
      float depth = vDisplacement * uFoldIntensity;
      color *= 0.95 + depth * 0.15;


      // ---------------------------------------------
      // Subtle grain for texture
      // ---------------------------------------------
      float grain = (snoise(uv * 400.0 + t * 5.0) - 0.5) * uGrain;
      color += grain;

      color = clamp(color, 0.0, 1.0);

      float alpha = 1.0;
      if (uUseTextMask > 0.5) {
        alpha = texture2D(uTextMask, vUv).a;
      }

      gl_FragColor = vec4(color, alpha);
    }
  `;

  // NEBULA - Realistic cosmic nebula with gas clouds and stars
  const NEBULA_FRAGMENT = `
    uniform float uTime;
    uniform vec2 uMouse;
    uniform vec2 uMouseVelocity;
    uniform vec2 uDisplacement;
    uniform float uPulse;
    uniform vec2 uResolution;
    uniform vec3 uColors[4];
    uniform float uColorBlend;
    uniform float uColorBalance[4];
    uniform float uDirection;
    uniform float uSpeed;
    uniform float uRimLight;
    uniform float uRimIntensity;
    uniform vec3 uRimColor;
    uniform float uRimFalloff;
    uniform float uGrain;
    uniform float uFoldIntensity;
    uniform float uFoldScale;
    uniform float uFoldSpeed;
    uniform float uScrollOffset;
    uniform sampler2D uTextMask;
    uniform float uUseTextMask;

    varying vec2 vUv;
    varying vec3 vNormal;
    varying vec3 vPosition;
    varying float vDisplacement;

    ${NOISE_GLSL}

    float hash(vec2 p) {
      return fract(sin(dot(p, vec2(127.1, 311.7))) * 43758.5453);
    }

    float fbm(vec2 p) {
        float v = 0.0;
        float a = 0.5;
        mat2 rot = mat2(cos(0.5), sin(0.5), -sin(0.5), cos(0.5));
        for (int i = 0; i < 5; ++i) {
            v += a * snoise(p);
            p = rot * p * 2.0 + vec2(100.0);
            a *= 0.5;
        }
        return v;
    }

    float fbmDetail(vec2 p, float detail) {
        float v = 0.0;
        float a = 0.5;
        float totalAmp = 0.0;
        mat2 rot = mat2(cos(0.5), sin(0.5), -sin(0.5), cos(0.5));
        
        float octaveFloat = 2.0 + detail * 1.0;
        int maxOctaves = int(ceil(octaveFloat));
        float lastOctaveWeight = fract(octaveFloat);
        
        float persistence = 0.45 + detail * 0.02;
        
        for (int i = 0; i < 7; ++i) {
            if (i >= maxOctaves) break;
            
            float weight = (i == maxOctaves - 1) ? lastOctaveWeight : 1.0;
            v += a * snoise(p) * weight;
            totalAmp += a * weight;
            
            p = rot * p * 2.0 + vec2(100.0);
            a *= persistence;
        }
        return v / max(totalAmp, 0.001);
    }

    void main() {
      vec2 uv = vUv * 0.8; 
      float aspect = uResolution.x / uResolution.y;
      uv.x *= aspect;

      float t = uTime * uSpeed * uFoldSpeed * 0.02;
      
      float cloudDetail = uFoldScale;
      float rawRichness = uFoldIntensity * 2.0;
      float cloudRichness = 0.5 + tanh(rawRichness) * 1.5;
      
      // ---------------------------------------------
      // MOUSE DISPLACEMENT - Persistent push effect
      // ---------------------------------------------
      vec2 toMouse = vUv - uMouse;
      float mouseDist = length(toMouse);
      
      vec2 displaceOffset = uDisplacement * 0.02;
      
      float pushRadius = 0.35;
      float pushStrength = smoothstep(pushRadius, 0.0, mouseDist);
      vec2 mouseOffset = (uMouse - vec2(0.5)) * 2.0;
      
      vec2 proximityPush = mouseOffset * pushStrength * 0.15;
      
      float pulseWave = uPulse * (1.0 - smoothstep(0.0, 0.5, mouseDist - uPulse * 0.8));
      vec2 pulsePush = mouseOffset * pulseWave * 0.25;
      
      vec2 totalDisplace = displaceOffset + proximityPush + pulsePush;
      
      float scrollY = uScrollOffset;

      // ---------------------------------------------
      // Direction-based time offset
      // ---------------------------------------------
      vec2 dirOffset = vec2(1.0, 1.0);
      if (uDirection == 1.0) dirOffset = vec2(0.0, 1.0);      // up
      else if (uDirection == 2.0) dirOffset = vec2(0.0, -1.0); // down
      else if (uDirection == 3.0) dirOffset = vec2(-1.0, 0.0); // left
      else if (uDirection == 4.0) dirOffset = vec2(1.0, 0.0);  // right

      // ---------------------------------------------
      // Color blending helper
      // ---------------------------------------------
      float blendSharpness = uColorBlend == 1.0 ? 0.1 : (uColorBlend == 2.0 ? 0.02 : 0.35);

      // ---------------------------------------------
      // Nebula Layers with displacement
      // ---------------------------------------------
      
      vec3 color = uColors[0] * uColorBalance[0];
      
      vec2 uv0 = uv * 0.25 + vec2(t * 0.06 * dirOffset.x, -t * 0.03 * dirOffset.y + scrollY * 0.2);
      float n0 = fbmDetail(uv0 + vec2(-100.0, 50.0), cloudDetail);
      n0 = n0 * 0.5 + 0.5;
      float mask0 = smoothstep(0.25, 0.25 + blendSharpness, n0);
      color += uColors[2] * uColorBalance[2] * 0.15 * mask0 * cloudRichness;
      
      vec2 uv1 = uv * 0.8 + vec2(t * 0.5 * dirOffset.x, t * 0.2 * dirOffset.y + scrollY * 0.4) + totalDisplace * 0.3;
      float n1 = fbmDetail(uv1, cloudDetail);
      n1 = n1 * 0.5 + 0.5;
      float mask1 = smoothstep(0.4, 0.4 + blendSharpness, n1);
      color += uColors[1] * uColorBalance[1] * mask1 * 0.35 * cloudRichness;

      vec2 uv2 = uv * 1.5 + vec2(-t * 0.3 * dirOffset.x, t * 0.4 * dirOffset.y + scrollY * 0.6) + totalDisplace * 0.6;
      float n2 = fbmDetail(uv2 + vec2(10.0), cloudDetail);
      n2 = n2 * 0.5 + 0.5;
      float mask2 = smoothstep(0.45, 0.45 + blendSharpness * 0.7, n2);
      color += uColors[2] * uColorBalance[2] * mask2 * 0.4 * cloudRichness;

      vec2 uv3 = uv * 3.0 + vec2(t * 0.2 * dirOffset.x, -t * 0.5 * dirOffset.y + scrollY * 0.8) + totalDisplace * 1.0;
      float n3 = fbmDetail(uv3 + vec2(20.0), cloudDetail);
      n3 = n3 * 0.5 + 0.5;
      float mask3 = smoothstep(0.55, 0.55 + blendSharpness * 0.7, n3);
      color += uColors[3] * uColorBalance[3] * mask3 * 0.4 * cloudRichness;

      // ---------------------------------------------
      // Dark Dust Lanes - displaced (softened)
      // ---------------------------------------------
      vec2 uvDust = uv * 2.0 + vec2(t * 0.1, scrollY * 0.5) + totalDisplace * 0.5;
      float dust = fbmDetail(uvDust, cloudDetail * 0.5);
      dust = dust * 0.5 + 0.5;
      float dustMask = smoothstep(0.4, 0.7, dust) * 0.3;
      color = mix(color, uColors[0], dustMask);

      // ---------------------------------------------
      // Pulse aurora effect
      // ---------------------------------------------
      if (uPulse > 0.01) {
        float pulseAurora = uPulse * smoothstep(0.4, 0.0, abs(mouseDist - uPulse * 0.6));
        color += uColors[3] * pulseAurora * 0.5;
      }

      // ---------------------------------------------
      // Rim Aurora - adds aurora to bright areas (controlled by uRimLight toggle)
      // ---------------------------------------------
      if (uRimLight > 0.5) {
        float luminance = dot(color, vec3(0.299, 0.587, 0.114));
        float auroraThreshold = 1.0 - uRimFalloff * 0.15;
        float aurora = smoothstep(auroraThreshold, auroraThreshold + 0.3, luminance);
        vec3 rimContribution = uRimColor * aurora * uRimIntensity * 2.0;
        rimContribution += uRimColor * luminance * uRimIntensity * 0.3;
        color += rimContribution;
      }

      // ---------------------------------------------
      // Finishing
      // ---------------------------------------------
      float grain = (hash(vUv * 90.0 + uTime) - 0.5) * uGrain;
      color += grain;
      
      color = pow(color, vec3(1.3));

      float alpha = 1.0;
      if (uUseTextMask > 0.5) {
        alpha = texture2D(uTextMask, vUv).a;
      }

      gl_FragColor = vec4(clamp(color, 0.0, 1.0), alpha);
    }
  `;

  // FLORAL - Abstract HD flower petal pattern with whispy, smokey effects
  const FLORAL_FRAGMENT = `
    uniform float uTime;
    uniform vec2 uMouse;
    uniform vec2 uMouseVelocity;
    uniform vec2 uDisplacement;
    uniform float uPulse;
    uniform vec2 uLastClickPos;
    uniform vec2 uResolution;
    uniform vec3 uColors[7];
    uniform float uColorBlend;
    uniform float uColorBalance[7];
    uniform float uDirection;
    uniform float uSpeed;
    uniform float uRimLight;
    uniform float uRimIntensity;
    uniform vec3 uRimColor;
    uniform float uRimFalloff;
    uniform float uGrain;
    uniform float uFoldIntensity;
    uniform float uFoldScale;
    uniform float uFoldSpeed;
    uniform float uReactiveStrength;
    uniform float uMouseRadius;
    uniform float uScrollOffset;
    uniform sampler2D uTextMask;
    uniform float uUseTextMask;

    varying vec2 vUv;
    varying vec3 vNormal;
    varying vec3 vPosition;
    varying float vDisplacement;

    ${NOISE_GLSL}

    float petalShape(vec2 p, float angle, float size) {
      float c = cos(angle);
      float s = sin(angle);
      vec2 rotP = vec2(p.x * c - p.y * s, p.x * s + p.y * c);
      
      float dist = length(rotP);
      
      float angleFromCenter = atan(rotP.x, max(rotP.y, 0.001));
      float curveFactor = 1.0 + sin(angleFromCenter * 1.5) * 0.2;
      
      float petalY = max(rotP.y, 0.0);
      
      float taper = 1.0 - (petalY / size);
      float petalWidth = abs(rotP.x) * 1.2 * curveFactor * taper;
      float petalLength = petalY * 0.5;
      
      float petal = 1.0 - smoothstep(size * 0.3, size * 0.6, petalWidth + petalLength);
      
      float tip = smoothstep(size * 1.6, size * 0.6, dist);
      petal *= tip;
      
      petal = pow(petal, 1.8);
      
      return petal;
    }
    
    float flowerPattern(vec2 uv, vec2 center, int petals, float rotation, float size) {
      vec2 p = uv - center;
      float dist = length(p);
      
      float pattern = 0.0;
      float petalAngle = 6.28318 / float(petals);
      
      for(int i = 0; i < 6; i++) {
        if(i >= petals) break;
        float a = float(i) * petalAngle + rotation;
        pattern = max(pattern, petalShape(p, a, size));
      }
      
      float center_circle = 1.0 - smoothstep(size * 0.12, size * 0.22, dist);
      pattern = max(pattern, center_circle);
      
      return pattern;
    }
    
    float petalTexture(vec2 p, float time) {
      float n1 = snoise(p * 6.0 + time * 0.1);
      float n2 = snoise(p * 12.0 - time * 0.08);
      return (n1 * 0.6 + n2 * 0.4) * 0.5 + 0.5;
    }

    void main() {
      vec2 uv = vUv;
      float aspect = uResolution.x / uResolution.y;
      uv.x *= aspect;
      
      float t = uTime * uSpeed * uFoldSpeed * 0.5;
      
      float flowerScale = uFoldScale;
      float flowerIntensity = uFoldIntensity;
      
      vec2 toMouse = vUv - uMouse;
      float mouseDist = length(toMouse);
      vec2 displaceOffset = uDisplacement * 0.02;
      
      float pushRadius = 0.4;
      float pushStrength = smoothstep(pushRadius, 0.0, mouseDist) * uReactiveStrength;
      vec2 mouseOffset = (uMouse - vec2(0.5)) * 2.0;
      vec2 proximityPush = mouseOffset * pushStrength * 0.08;
      
      vec2 clickDisplace = vec2(0.0);
      if (uPulse > 0.01) {
        float clickDist = distance(vUv, uLastClickPos);
        float expandProgress = 1.0 - uPulse;
        
        float angleToClick = atan(vUv.y - uLastClickPos.y, vUv.x - uLastClickPos.x);
        float wavePattern = sin(angleToClick * 3.0 + expandProgress * 6.0) * 0.04 + 
                           sin(angleToClick * 5.0 - expandProgress * 9.0) * 0.03 +
                           sin(angleToClick * 2.0 + expandProgress * 4.0) * 0.02;
        float waveRadius = expandProgress * uMouseRadius * 3.0 * (1.0 + wavePattern);
        float waveThickness = uMouseRadius * 1.2;
        float wave = smoothstep(waveRadius + waveThickness, waveRadius, clickDist) * 
                     smoothstep(waveRadius - waveThickness * 0.5, waveRadius, clickDist);
        float clickInfluence = wave * uPulse;
        vec2 clickOffset = (uLastClickPos - vec2(0.5)) * 2.0;
        clickDisplace = clickInfluence * clickOffset * 0.6;
      }
      
      vec2 totalDisplace = displaceOffset + proximityPush + clickDisplace;
      
      float scrollY = uScrollOffset;
      
      vec2 dirOffset = vec2(1.0, 1.0);
      if (uDirection == 1.0) dirOffset = vec2(0.0, 1.0);
      else if (uDirection == 2.0) dirOffset = vec2(0.0, -1.0);
      else if (uDirection == 3.0) dirOffset = vec2(1.0, 0.0);
      else if (uDirection == 4.0) dirOffset = vec2(-1.0, 0.0);
      
      float blendSharpness = uColorBlend == 1.0 ? 0.15 : (uColorBlend == 2.0 ? 0.05 : 0.35);
      float edgePow = blendSharpness < 0.1 ? 3.0 : (blendSharpness < 0.25 ? 2.2 : 1.5);
      
      vec3 color = uColors[0] * uColorBalance[0] * 0.2;
      
      vec2 uv1 = uv * (1.5 / flowerScale) + vec2(t * 0.1 * dirOffset.x, t * 0.08 * dirOffset.y + scrollY * 0.3) + totalDisplace * 0.3;
      vec2 tile1 = fract(uv1) - 0.5;
      vec2 id1 = floor(uv1);
      float hash1 = fract(sin(dot(id1, vec2(127.1, 311.7))) * 43758.5453);
      float hash1b = fract(sin(dot(id1, vec2(269.5, 183.3))) * 43758.5453);
      int petals1 = 6 + int(hash1 * 3.0);
      float rotation1 = hash1 * 6.28318 + t * 0.15;
      float sizeVar1 = 0.35 * (0.8 + hash1b * 0.4);
      float flower1 = flowerPattern(tile1, vec2(0.0), petals1, rotation1, sizeVar1);
      flower1 = pow(flower1, edgePow);
      float texture1 = petalTexture(tile1, t);
      color += uColors[1] * uColorBalance[1] * flower1 * (0.6 + texture1 * 0.3) * flowerIntensity;
      
      vec2 uv2 = uv * (2.2 / flowerScale) + vec2(-t * 0.12 * dirOffset.x, t * 0.1 * dirOffset.y + scrollY * 0.5) + totalDisplace * 0.5;
      uv2 += vec2(0.5, 0.25);
      vec2 tile2 = fract(uv2) - 0.5;
      vec2 id2 = floor(uv2);
      float hash2 = fract(sin(dot(id2, vec2(269.5, 183.3))) * 43758.5453);
      float hash2b = fract(sin(dot(id2, vec2(419.2, 371.9))) * 43758.5453);
      int petals2 = 5 + int(hash2 * 4.0);
      float rotation2 = hash2 * 6.28318 - t * 0.18;
      float sizeVar2 = 0.32 * (0.8 + hash2b * 0.4);
      float flower2 = flowerPattern(tile2, vec2(0.0), petals2, rotation2, sizeVar2);
      flower2 = pow(flower2, edgePow);
      float texture2 = petalTexture(tile2, t * 1.1);
      color += uColors[2] * uColorBalance[2] * flower2 * (0.65 + texture2 * 0.25) * flowerIntensity;
      
      vec2 uv3 = uv * (2.8 / flowerScale) + vec2(t * 0.08 * dirOffset.x, -t * 0.12 * dirOffset.y + scrollY * 0.7) + totalDisplace * 0.7;
      vec2 tile3 = fract(uv3) - 0.5;
      vec2 id3 = floor(uv3);
      float hash3 = fract(sin(dot(id3, vec2(419.2, 371.9))) * 43758.5453);
      float hash3b = fract(sin(dot(id3, vec2(541.3, 239.7))) * 43758.5453);
      int petals3 = 6 + int(hash3 * 3.0);
      float rotation3 = hash3 * 6.28318 + t * 0.22;
      float sizeVar3 = 0.3 * (0.8 + hash3b * 0.4);
      float flower3 = flowerPattern(tile3, vec2(0.0), petals3, rotation3, sizeVar3);
      flower3 = pow(flower3, edgePow);
      float texture3 = petalTexture(tile3, t * 0.9);
      color += uColors[3] * uColorBalance[3] * flower3 * (0.7 + texture3 * 0.3) * flowerIntensity;
      
      vec2 uv4 = uv * (3.5 / flowerScale) + vec2(t * 0.15 * dirOffset.x, t * 0.1 * dirOffset.y + scrollY * 0.9) + totalDisplace;
      uv4 += vec2(0.25, 0.5);
      vec2 tile4 = fract(uv4) - 0.5;
      vec2 id4 = floor(uv4);
      float hash4 = fract(sin(dot(id4, vec2(541.3, 239.7))) * 43758.5453);
      float hash4b = fract(sin(dot(id4, vec2(631.2, 152.4))) * 43758.5453);
      int petals4 = 5 + int(hash4 * 3.0);
      float rotation4 = hash4 * 6.28318 - t * 0.28;
      float sizeVar4 = 0.28 * (0.8 + hash4b * 0.4);
      float flower4 = flowerPattern(tile4, vec2(0.0), petals4, rotation4, sizeVar4);
      flower4 = pow(flower4, edgePow);
      float texture4 = petalTexture(tile4, t * 1.2);
      color += uColors[4] * uColorBalance[4] * flower4 * (0.75 + texture4 * 0.25) * flowerIntensity;
      color += uColors[5] * uColorBalance[5] * flower4 * (0.5 + texture4 * 0.2) * flowerIntensity;
      color += uColors[6] * uColorBalance[6] * flower4 * (0.4 + texture4 * 0.15) * flowerIntensity;
      
      if (uRimLight > 0.5) {
        float luminance = dot(color, vec3(0.299, 0.587, 0.114));
        float rimThreshold = 1.0 - uRimFalloff * 0.1;
        float rim = smoothstep(rimThreshold, rimThreshold + 0.2, luminance);
        color += uRimColor * rim * uRimIntensity * 1.5;
      }
      
      float grain = (fract(sin(dot(vUv, vec2(12.9898, 78.233))) * 43758.5453) - 0.5) * uGrain;
      color += grain;
      
      color = pow(color, vec3(1.1));
      
      float alpha = 1.0;
      if (uUseTextMask > 0.5) {
        alpha = texture2D(uTextMask, vUv).a;
      }

      gl_FragColor = vec4(clamp(color, 0.0, 1.0), alpha);
    }
  `;

  // OIL - Oily oil refraction with fabric-like depth
  const OIL_FRAGMENT = `
    uniform float uTime;
    uniform vec2 uMouse;
    uniform vec2 uResolution;
    uniform vec3 uColors[7];
    uniform float uColorBlend;
    uniform float uColorBalance[7];
    uniform float uDirection;
    uniform float uSpeed;
    uniform float uRimLight;
    uniform float uRimIntensity;
    uniform vec3 uRimColor;
    uniform float uRimFalloff;
    uniform float uGrain;
    uniform float uFoldIntensity;
    uniform float uFoldScale;
    uniform float uFoldSpeed;
    uniform float uReactiveStrength;
    uniform float uMouseRadius;
    uniform vec2 uDisplacement;
    uniform float uScrollOffset;
    uniform float uPulse;
    uniform vec2 uLastClickPos;
    uniform sampler2D uTextMask;
    uniform float uUseTextMask;

    varying vec2 vUv;
    varying vec3 vNormal;
    varying vec3 vPosition;
    varying float vDisplacement;

    ${NOISE_GLSL}
    
    float floralNoise(vec2 p, float t) {
      vec2 q = vec2(
        fbm(p + vec2(0.0, 0.0), 4),
        fbm(p + vec2(5.2, 1.3), 4)
      );
      
      vec2 r = vec2(
        fbm(p + 0.85*q + vec2(t, t*0.9), 4),
        fbm(p + 0.85*q + vec2(t*0.8, -t) + vec2(8.3, 2.8), 4)
      );
      
      return fbm(p + 0.85*r, 4);
    }

    void main() {
      vec2 uv = vUv;
      vec2 aspectUv = uv;
      aspectUv.x *= uResolution.x / uResolution.y;

      float t = uTime * uSpeed * uFoldSpeed * 0.1;
      
      vec2 dirOffset = vec2(1.0, 1.0);
      if (uDirection == 1.0) dirOffset = vec2(0.0, -1.0);
      else if (uDirection == 2.0) dirOffset = vec2(0.0, 1.0);
      else if (uDirection == 3.0) dirOffset = vec2(1.0, 0.0);
      else if (uDirection == 4.0) dirOffset = vec2(-1.0, 0.0);
      
      // ---------------------------------------------
      // Interaction & Displacement
      // ---------------------------------------------
      vec2 mousePos = uMouse;
      float mouseDist = distance(uv, mousePos);
      float mouseInfluence = smoothstep(uMouseRadius * 1.5, 0.0, mouseDist) * uReactiveStrength;
      
      float scaleFactor = uFoldScale * 0.1 + pow(uFoldScale, 2.0) * 0.2;
      
      vec2 flowUv = aspectUv * scaleFactor;
      
      flowUv += vec2(t * 0.15 * dirOffset.x, t * 0.15 * dirOffset.y);
      flowUv.y += uScrollOffset * 0.08;
      
      flowUv += uDisplacement * 0.025;
      vec2 toMouse = uv - mousePos;
      float pushDist = length(toMouse);
      float pushFalloff = smoothstep(0.0, uMouseRadius * 0.3, pushDist);
      float pushStrength = mouseInfluence * pushFalloff * 0.12;
      if (pushDist > 0.001) {
        vec2 pushDir = toMouse / pushDist;
        flowUv += pushDir * pushStrength;
      }

      // ---------------------------------------------
      // Height Field & Normals
      // ---------------------------------------------
      float h = floralNoise(flowUv, t);
      
      float clickDist = distance(uv, uLastClickPos);
      float ripple = sin(clickDist * 12.0 - uPulse * 10.0) * 0.5 + 0.5;
      ripple *= smoothstep(0.8, 0.0, clickDist);
      ripple *= smoothstep(0.0, 0.2, uPulse) * smoothstep(1.0, 0.6, uPulse);
      h += ripple * 0.6;
      
      float normalStrength = (3.5 + uFoldIntensity * 8.0) / max(scaleFactor, 0.001);
      vec2 dxy = vec2(dFdx(h), dFdy(h)) * normalStrength;
      dxy = mix(dxy, vec2(0.0), 0.08);
      vec3 normal = normalize(vec3(-dxy.x, -dxy.y, 1.0));

      // ---------------------------------------------
      // Lighting Model (Oillic)
      // ---------------------------------------------
      vec3 viewDir = normalize(vec3(0.0, 0.0, 1.0));
      vec3 lightDir = normalize(vec3(-0.5, 0.5, 1.0));
      vec3 halfDir = normalize(lightDir + viewDir);
      
      float NdotH = max(0.0, dot(normal, halfDir));
      float spec = pow(NdotH, 30.0);
      
      float specSoft = pow(NdotH, 4.0);
      
      float fresnel = pow(1.0 - max(0.0, dot(normal, viewDir)), 2.0);

      // ---------------------------------------------
      // Color Composition
      // ---------------------------------------------
      float blendSharpness = uColorBlend == 1.0 ? 0.15 : (uColorBlend == 2.0 ? 0.05 : 0.35);
      
      vec3 baseColor = uColors[0] * uColorBalance[0];
      
      vec3 refDir = reflect(-viewDir, normal);
      float envMap = snoise(refDir.xy * 2.0) * 0.5 + 0.5;
      
      float sharpPow = blendSharpness < 0.1 ? 6.0 : (blendSharpness < 0.25 ? 2.5 : 1.0);
      float envMapSharp = pow(envMap, sharpPow);

      if (blendSharpness < 0.1) {
        envMapSharp = envMapSharp > 0.5 ? 1.0 : 0.0;
      }
      
      vec3 envColor = mix(
        uColors[1] * uColorBalance[1], 
        uColors[2] * uColorBalance[2], 
        envMapSharp
      );
      
      vec3 finalColor = baseColor;
      
      finalColor = mix(finalColor, envColor, specSoft * 0.6);
      
      finalColor += envColor * fresnel * 0.8;
      
      finalColor += vec3(1.0) * spec * 0.8;
      
      if (length(uColors[3]) > 0.1) {
        finalColor += uColors[3] * uColorBalance[3] * specSoft * 0.4;
      }
      
      // ---------------------------------------------
      // Chromatic Edge Highlights (using user colors)
      // ---------------------------------------------
      float chromatic = dot(normal, vec3(1.0, 0.0, 0.0));
      vec3 edgeColor = vec3(0.0);
      
      edgeColor += uColors[1] * uColorBalance[1] * smoothstep(0.2, 0.6, chromatic);
      edgeColor += uColors[2] * uColorBalance[2] * smoothstep(0.2, 0.6, -chromatic);
      
      finalColor += edgeColor * fresnel * uRimIntensity * 0.5;

      // ---------------------------------------------
      // Rim Light (User Controlled)
      // ---------------------------------------------
      if (uRimLight > 0.5) {
        float heightPeaks = smoothstep(0.6, 0.8, h * 0.5 + 0.5);
        float rim = heightPeaks * uRimIntensity;
        finalColor += uRimColor * rim;
      }

      // ---------------------------------------------
      // Grain & Dithering
      // ---------------------------------------------
      float grain = (snoise(uv * 1000.0 + t * 10.0) - 0.5) * uGrain;
      finalColor += grain;
      
      finalColor = pow(finalColor, vec3(1.1));

      float alpha = 1.0;
      if (uUseTextMask > 0.5) {
        alpha = texture2D(uTextMask, vUv).a;
      }
      gl_FragColor = vec4(clamp(finalColor, 0.0, 1.0), alpha);
    }
  `;

  // DIVA - Iridescent flowing gradients like Aurora
  const DIVA_FRAGMENT = `
    uniform float uTime;
    uniform vec2 uMouse;
    uniform vec2 uMouseVelocity;
    uniform vec2 uDisplacement;
    uniform float uPulse;
    uniform vec2 uLastClickPos;
    uniform vec2 uResolution;
    uniform vec3 uColors[7];
    uniform float uColorBlend;
    uniform float uColorBalance[7];
    uniform float uDirection;
    uniform float uSpeed;
    uniform float uRimLight;
    uniform float uRimIntensity;
    uniform vec3 uRimColor;
    uniform float uRimFalloff;
    uniform float uGrain;
    uniform float uFoldIntensity;
    uniform float uFoldScale;
    uniform float uFoldSpeed;
    uniform float uReactiveStrength;
    uniform float uMouseRadius;
    uniform float uScrollOffset;
    uniform sampler2D uTextMask;
    uniform float uUseTextMask;

    varying vec2 vUv;
    varying vec3 vNormal;
    varying vec3 vPosition;
    varying float vDisplacement;

    ${NOISE_GLSL}

    float divaFlow(vec2 uv, float t, vec2 dir) {
      float flow = 0.0;
      flow += sin(uv.x * 3.0 + t * 0.5 * dir.x) * sin(uv.y * 2.5 - t * 0.4 * dir.y);
      flow += sin(uv.x * 2.0 - t * 0.3 * dir.x + uv.y * 3.0) * 0.7;
      flow += sin((uv.x * dir.x + uv.y * dir.y) * 2.5 + t * 0.4) * 0.5;
      return flow * 0.3 + 0.5;
    }
    
    vec3 rainbowSpectrum(float phase, vec3 colors[7], float balance[7], float blendSharpness) {
      float index = phase * 7.0;
      float blend = fract(index);
      int i1 = int(floor(index)) % 7;
      int i2 = (i1 + 1) % 7;
      
      if (blendSharpness < 0.1) {
        blend = blend > 0.5 ? 1.0 : 0.0;
      } else if (blendSharpness < 0.25) {
        blend = blend * blend * blend * (blend * (blend * 6.0 - 15.0) + 10.0);
      } else {
        blend = blend * blend * (3.0 - 2.0 * blend);
      }
      
      return mix(
        colors[i1] * balance[i1],
        colors[i2] * balance[i2],
        blend
      );
    }

    void main() {
      vec2 uv = vUv;
      float aspect = uResolution.x / uResolution.y;
      
      float t = uTime * uSpeed * uFoldSpeed * 0.15;
      
      vec2 dirOffset = vec2(1.0, 1.0);
      if (uDirection == 1.0) dirOffset = vec2(0.0, 1.0);      // up
      else if (uDirection == 2.0) dirOffset = vec2(0.0, -1.0); // down
      else if (uDirection == 3.0) dirOffset = vec2(-1.0, 0.0); // left
      else if (uDirection == 4.0) dirOffset = vec2(1.0, 0.0);  // right

      vec2 mousePos = uMouse;
      float mouseDist = distance(uv, mousePos);
      float mouseInfluence = smoothstep(uMouseRadius * 2.0, 0.0, mouseDist) * uReactiveStrength;
      
      vec2 displaceOffset = uDisplacement * 0.15;
      float scrollY = uScrollOffset;

      // ---------------------------------------------
      // Create flowing diva patterns
      // ---------------------------------------------
      
      vec2 flowUV1 = uv * 1.8 * uFoldScale + vec2(t * 0.3 * dirOffset.x, t * 0.25 * dirOffset.y + scrollY * 0.3) + displaceOffset;
      vec2 flowUV2 = uv * 2.5 * uFoldScale + vec2(-t * 0.25 * dirOffset.x, t * 0.35 * dirOffset.y + scrollY * 0.5) + displaceOffset * 0.7;
      vec2 flowUV3 = uv * 1.2 * uFoldScale + vec2(t * 0.2 * dirOffset.x, -t * 0.2 * dirOffset.y + scrollY * 0.2) + displaceOffset * 1.2;
      
      float flow1 = divaFlow(flowUV1, t * 2.0, dirOffset);
      float flow2 = divaFlow(flowUV2 + vec2(50.0), t * 1.8, dirOffset);
      float flow3 = divaFlow(flowUV3 + vec2(100.0), t * 2.2, dirOffset);
      
      float noiseDistort = snoise(uv * 2.0 * uFoldScale + t * 0.2) * 0.2;
      
      // ---------------------------------------------
      // Diva color dispersion
      // ---------------------------------------------
      
      float blendSharpness = uColorBlend == 1.0 ? 0.15 : (uColorBlend == 2.0 ? 0.05 : 0.35);
      
      float phase1 = fract(flow1 + noiseDistort + mouseInfluence * 0.5);
      float phase2 = fract(flow2 + noiseDistort * 0.8 - mouseInfluence * 0.3);
      float phase3 = fract(flow3 + noiseDistort * 0.6);
      
      vec3 color = uColors[0] * uColorBalance[0];
      float bgLuminance = dot(color, vec3(0.299, 0.587, 0.114));
      bool isLightBg = bgLuminance > 0.5;
      
      vec3 rainbow1 = rainbowSpectrum(phase1, uColors, uColorBalance, blendSharpness);
      float intensity1 = flow1 * uFoldIntensity;
      
      if (isLightBg) {
        color = mix(color, rainbow1, intensity1 * 0.7);
      } else {
        color = mix(color, color + rainbow1 * 0.75, intensity1 * 0.65);
      }
      
      vec3 rainbow2 = rainbowSpectrum(phase2, uColors, uColorBalance, blendSharpness);
      float intensity2 = flow2 * uFoldIntensity * 0.75;
      
      if (isLightBg) {
        color = mix(color, rainbow2, intensity2 * 0.5);
      } else {
        color = mix(color, color + rainbow2 * 0.65, intensity2 * 0.45);
      }
      
      vec3 rainbow3 = rainbowSpectrum(phase3, uColors, uColorBalance, blendSharpness);
      float intensity3 = flow3 * uFoldIntensity * 0.55;
      
      if (isLightBg) {
        color = mix(color, rainbow3, intensity3 * 0.35);
      } else {
        color = mix(color, color + rainbow3 * 0.55, intensity3 * 0.3);
      }
      
      // ---------------------------------------------
      // Displacement depth
      // ---------------------------------------------
      float depthFactor = abs(vDisplacement) * uFoldIntensity * 0.4;
      color = mix(color, color * 1.15, depthFactor);
      
      if (mouseInfluence > 0.01) {
        color += color * mouseInfluence * 0.25;
      }
      
      if (uPulse > 0.01) {
        float clickDist = distance(vUv, uLastClickPos);
        float expandProgress = 1.0 - uPulse;
        float waveRadius = expandProgress * uMouseRadius * 3.5;
        float waveThickness = uMouseRadius * 0.8;
        
        float wave = smoothstep(waveRadius + waveThickness, waveRadius, clickDist) * 
                     smoothstep(waveRadius - waveThickness * 0.5, waveRadius, clickDist);
        
        float clickIntensity = wave * uPulse * 1.2;
        color += color * clickIntensity;
      }
      
      // ---------------------------------------------
      // Rim lighting
      // ---------------------------------------------
      if (uRimLight > 0.5) {
        float edgeFactor = 1.0 - abs(dot(normalize(vNormal), vec3(0.0, 0.0, 1.0)));
        edgeFactor = pow(edgeFactor, uRimFalloff);
        color += uRimColor * edgeFactor * uRimIntensity;
      }
      
      // ---------------------------------------------
      // Grain
      // ---------------------------------------------
      float grain = (snoise(uv * 1000.0 + t * 10.0) - 0.5) * uGrain;
      color += grain;
      
      color = pow(color, vec3(1.1));

      float alpha = 1.0;
      if (uUseTextMask > 0.5) {
        alpha = texture2D(uTextMask, vUv).a;
      }

      gl_FragColor = vec4(clamp(color, 0.0, 1.0), alpha);
    }
  `;

  // PIXEL - Dithered pixel pattern effect with FBM noise
  const PIXEL_FRAGMENT = `
    uniform float uTime;
    uniform vec2 uMouse;
    uniform vec2 uMouseVelocity;
    uniform vec2 uDisplacement;
    uniform float uPulse;
    uniform vec2 uResolution;
    uniform vec3 uColors[7];
    uniform float uColorBlend;
    uniform float uColorBalance[7];
    uniform float uDirection;
    uniform float uSpeed;
    uniform float uRimLight;
    uniform float uRimIntensity;
    uniform vec3 uRimColor;
    uniform float uRimFalloff;
    uniform float uGrain;
    uniform float uFoldIntensity;
    uniform float uFoldScale;
    uniform float uFoldSpeed;
    uniform float uReactiveStrength;
    uniform float uMouseRadius;
    uniform float uScrollOffset;
    uniform float uScrollVelocity;
    uniform sampler2D uTextMask;
    uniform float uUseTextMask;

    varying vec2 vUv;
    varying vec3 vNormal;
    varying vec3 vPosition;
    varying float vDisplacement;

    float Bayer2(vec2 a) {
      a = floor(a);
      return fract(a.x / 2.0 + a.y * a.y * 0.75);
    }
    #define Bayer4(a) (Bayer2(0.5 * (a)) * 0.25 + Bayer2(a))
    #define Bayer8(a) (Bayer4(0.5 * (a)) * 0.25 + Bayer2(a))

    float hash11(float n) { return fract(sin(n) * 43758.5453); }
    
    float hash12(vec2 p) {
      return fract(sin(dot(p, vec2(127.1, 311.7))) * 43758.5453);
    }

    float vnoise(vec3 p) {
      vec3 ip = floor(p);
      vec3 fp = fract(p);
      float n000 = hash11(dot(ip + vec3(0.0, 0.0, 0.0), vec3(1.0, 57.0, 113.0)));
      float n100 = hash11(dot(ip + vec3(1.0, 0.0, 0.0), vec3(1.0, 57.0, 113.0)));
      float n010 = hash11(dot(ip + vec3(0.0, 1.0, 0.0), vec3(1.0, 57.0, 113.0)));
      float n110 = hash11(dot(ip + vec3(1.0, 1.0, 0.0), vec3(1.0, 57.0, 113.0)));
      float n001 = hash11(dot(ip + vec3(0.0, 0.0, 1.0), vec3(1.0, 57.0, 113.0)));
      float n101 = hash11(dot(ip + vec3(1.0, 0.0, 1.0), vec3(1.0, 57.0, 113.0)));
      float n011 = hash11(dot(ip + vec3(0.0, 1.0, 1.0), vec3(1.0, 57.0, 113.0)));
      float n111 = hash11(dot(ip + vec3(1.0, 1.0, 1.0), vec3(1.0, 57.0, 113.0)));
      vec3 w = fp * fp * fp * (fp * (fp * 6.0 - 15.0) + 10.0);
      float x00 = mix(n000, n100, w.x);
      float x10 = mix(n010, n110, w.x);
      float x01 = mix(n001, n101, w.x);
      float x11 = mix(n011, n111, w.x);
      float y0 = mix(x00, x10, w.y);
      float y1 = mix(x01, x11, w.y);
      return mix(y0, y1, w.z) * 2.0 - 1.0;
    }

    float fbm2(vec2 uv, float t) {
      vec3 p = vec3(uv * uFoldScale, t);
      float amp = 1.0;
      float freq = 1.0;
      float sum = 1.0;
      for (int i = 0; i < 5; ++i) {
        sum += amp * vnoise(p * freq);
        freq *= 1.25;
        amp *= 1.0;
      }
      return sum * 0.5 + 0.5;
    }

    float maskCircle(vec2 p, float cov) {
      float r = sqrt(cov) * 0.25;
      float d = length(p - 0.5) - r;
      float aa = 0.5 * fwidth(d);
      return cov * (1.0 - smoothstep(-aa, aa, d * 2.0));
    }

    float maskTriangle(vec2 p, vec2 id, float cov) {
      bool flip = mod(id.x + id.y, 2.0) > 0.5;
      if (flip) p.x = 1.0 - p.x;
      float r = sqrt(cov);
      float d = p.y - r * (1.0 - p.x);
      float aa = fwidth(d);
      return cov * clamp(0.5 - d / aa, 0.0, 1.0);
    }

    float maskDiamond(vec2 p, float cov) {
      float r = sqrt(cov) * 0.564;
      return step(abs(p.x - 0.49) + abs(p.y - 0.49), r);
    }

    void main() {
      float t = uTime * uSpeed * uFoldSpeed * 0.05;
      
      vec2 dirOffset = vec2(1.0, 1.0);
      if (uDirection == 1.0) dirOffset = vec2(0.0, -1.0);
      else if (uDirection == 2.0) dirOffset = vec2(0.0, 1.0);
      else if (uDirection == 3.0) dirOffset = vec2(1.0, 0.0);
      else if (uDirection == 4.0) dirOffset = vec2(-1.0, 0.0);

      float pixelSize = 3.0 + uFoldIntensity * 5.0;
      
      vec2 fragCoord = gl_FragCoord.xy;
      float aspectRatio = uResolution.x / uResolution.y;

      vec2 pixelId = floor(fragCoord / pixelSize);
      vec2 pixelUV = fract(fragCoord / pixelSize);

      float cellPixelSize = 8.0 * pixelSize;
      vec2 cellId = floor(fragCoord / cellPixelSize);
      vec2 cellCoord = cellId * cellPixelSize;
      vec2 uv = cellCoord / uResolution * vec2(aspectRatio, 1.0);
      
      uv += dirOffset * t * 0.5;
      
      uv.y += uScrollOffset * 0.02;
      
      uv += uDisplacement * 0.05;

      float base = fbm2(uv, t);
      base = base * 0.5 - 0.65;

      float density = 0.5 + uRimFalloff * 0.1;
      float feed = base + (density - 0.5) * 0.3;

      vec2 mousePos = uMouse;
      float mouseDist = distance(vUv, mousePos);
      float mouseInfluence = smoothstep(uMouseRadius * 2.0, 0.0, mouseDist) * uReactiveStrength;
      feed += mouseInfluence * 0.3;

      if (uPulse > 0.01) {
        vec2 mousePixelCoord = uMouse * uResolution;
        float elapsed = uPulse * 2.0;
        
        float pixelDist = distance(fragCoord, mousePixelCoord);
        
        float maxRadiusPixels = uMouseRadius * min(uResolution.x, uResolution.y);
        
        float pixelRandom = hash12(pixelId);
        
        float randomExpansionFactor = 0.5 + pixelRandom * 1.0;
        
        float turnOnTime = (pixelDist / maxRadiusPixels) * 0.6 * randomExpansionFactor;
        float isActive = step(turnOnTime, elapsed);
        
        float flickerStartTime = 0.8 + pixelRandom * 0.5;
        float flickerDuration = 1.0;
        float flickerProgress = clamp((elapsed - flickerStartTime) / flickerDuration, 0.0, 1.0);
        
        float timeFlicker = 1.0;
        if (elapsed > flickerStartTime) {
          float flickerSpeed = 20.0;
          timeFlicker = mix(0.3, 1.0, hash12(pixelId + floor(uTime * flickerSpeed)));
        }
        
        float intensity = (1.0 - flickerProgress) * timeFlicker;
        
        float distFalloff = exp(-pixelDist / (maxRadiusPixels * 0.3));
        
        float clickContribution = isActive * intensity * distFalloff * uReactiveStrength * 0.8;
        feed += clickContribution;
      }
      
      float bayer = Bayer8(fragCoord / pixelSize) - 0.5;
      float bw = step(0.5, feed + bayer);

      float h = hash12(floor(fragCoord / pixelSize));
      float jitterScale = 1.0 + (h - 0.5) * uGrain * 10.0;
      float coverage = bw * jitterScale;
      
      float M;
      int shapeType = int(uColorBlend);
      if (shapeType == 1) M = maskCircle(pixelUV, coverage);
      else if (shapeType == 2) M = maskTriangle(pixelUV, pixelId, coverage);
      else if (shapeType == 3) M = maskDiamond(pixelUV, coverage);
      else M = coverage;

      if (uRimIntensity > 0.0) {
        vec2 norm = gl_FragCoord.xy / uResolution;
        float edge = min(min(norm.x, norm.y), min(1.0 - norm.x, 1.0 - norm.y));
        float fade = smoothstep(0.0, uRimIntensity * 0.5, edge);
        M *= fade;
      }

      vec3 pixelColor = uColors[1] * uColorBalance[1];
      
      float colorVar = fbm2(uv * 0.5, t * 0.5);
      vec3 secondaryColor = uColors[2] * uColorBalance[2];
      pixelColor = mix(pixelColor, secondaryColor, colorVar * 0.3);
      
      vec3 accentColor = uColors[3] * uColorBalance[3];
      float accentMix = mouseInfluence * 0.5 + smoothstep(0.6, 0.9, feed) * 0.3;
      pixelColor = mix(pixelColor, accentColor, accentMix);
      
      if (uRimLight > 0.5) {
        float rimMask = 1.0 - smoothstep(0.3, 0.7, feed);
        pixelColor = mix(pixelColor, uRimColor, rimMask * 0.2);
      }

      vec3 bgColor = uColors[0] * uColorBalance[0];
      vec3 finalColor = mix(bgColor, pixelColor, M);

      float alpha = 1.0;
      if (uUseTextMask > 0.5) {
        alpha = texture2D(uTextMask, vUv).a;
      }
      
      gl_FragColor = vec4(clamp(finalColor, 0.0, 1.0), alpha);
    }
  `;

  // SHIMMER - Iridescent holographic effect with flowing color dispersion
  const SHIMMER_FRAGMENT = `
    uniform float uTime;
    uniform vec2 uMouse;
    uniform vec2 uMouseVelocity;
    uniform vec2 uDisplacement;
    uniform float uPulse;
    uniform vec2 uResolution;
    uniform vec3 uColors[7];
    uniform float uColorBlend;
    uniform float uColorBalance[7];
    uniform float uDirection;
    uniform float uSpeed;
    uniform float uRimLight;
    uniform float uRimIntensity;
    uniform vec3 uRimColor;
    uniform float uRimFalloff;
    uniform float uGrain;
    uniform float uFoldIntensity;
    uniform float uFoldScale;
    uniform float uFoldSpeed;
    uniform float uReactiveStrength;
    uniform float uMouseRadius;
    uniform float uScrollOffset;
    uniform sampler2D uTextMask;
    uniform float uUseTextMask;

    varying vec2 vUv;
    varying vec3 vNormal;
    varying vec3 vPosition;
    varying float vDisplacement;

    ${NOISE_GLSL}

    vec3 smoothSpectrum(float phase, vec3 colors[7], float balance[7], float blendSharpness) {
      float p = phase * 6.283185;
      
      float sharpnessMult = blendSharpness < 0.1 ? 4.0 : (blendSharpness < 0.25 ? 2.0 : 1.0);
      
      float w0 = max(0.0, cos(p - 0.0) * sharpnessMult * 0.5 + 0.5);
      float w1 = max(0.0, cos(p - 0.898) * sharpnessMult * 0.5 + 0.5);
      float w2 = max(0.0, cos(p - 1.795) * sharpnessMult * 0.5 + 0.5);
      float w3 = max(0.0, cos(p - 2.693) * sharpnessMult * 0.5 + 0.5);
      float w4 = max(0.0, cos(p - 3.590) * sharpnessMult * 0.5 + 0.5);
      float w5 = max(0.0, cos(p - 4.488) * sharpnessMult * 0.5 + 0.5);
      float w6 = max(0.0, cos(p - 5.385) * sharpnessMult * 0.5 + 0.5);
      
      if (blendSharpness < 0.1) {
        w0 = w0 > 0.5 ? 1.0 : 0.0;
        w1 = w1 > 0.5 ? 1.0 : 0.0;
        w2 = w2 > 0.5 ? 1.0 : 0.0;
        w3 = w3 > 0.5 ? 1.0 : 0.0;
        w4 = w4 > 0.5 ? 1.0 : 0.0;
        w5 = w5 > 0.5 ? 1.0 : 0.0;
        w6 = w6 > 0.5 ? 1.0 : 0.0;
      }
      
      float total = w0 + w1 + w2 + w3 + w4 + w5 + w6 + 0.001;
      
      vec3 result = 
        colors[0] * balance[0] * w0 +
        colors[1] * balance[1] * w1 +
        colors[2] * balance[2] * w2 +
        colors[3] * balance[3] * w3 +
        colors[4] * balance[4] * w4 +
        colors[5] * balance[5] * w5 +
        colors[6] * balance[6] * w6;
      
      return result / total;
    }

    void main() {
      float mr = min(uResolution.x, uResolution.y);
      vec2 uv = (vUv * 2.0 - 1.0) * uResolution.xy / mr;
      
      vec2 dirOffset = vec2(1.0, 1.0);
      if (uDirection == 1.0) dirOffset = vec2(0.0, 1.0);      // up
      else if (uDirection == 2.0) dirOffset = vec2(0.0, -1.0); // down
      else if (uDirection == 3.0) dirOffset = vec2(-1.0, 0.0); // left
      else if (uDirection == 4.0) dirOffset = vec2(1.0, 0.0);  // right
      
      vec2 mousePos = uMouse;
      float mouseDist = distance(vUv, mousePos);
      float mouseInfluence = smoothstep(uMouseRadius * 2.0, 0.0, mouseDist) * uReactiveStrength;
      uv += (mousePos - vec2(0.5)) * mouseInfluence * uFoldIntensity * 0.5;
      
      uv.y += uScrollOffset * 0.1;
      
      uv += uDisplacement * 0.08;

      float t = uTime * 0.5 * uSpeed * uFoldSpeed;
      
      float blendSharpness = uColorBlend == 1.0 ? 0.15 : (uColorBlend == 2.0 ? 0.05 : 0.35);
      
      vec2 uvFlow = uv;
      uvFlow.x += t * 0.2 * dirOffset.x;
      uvFlow.y += t * 0.2 * dirOffset.y;
      
      float d = -t;
      float a = 0.0;
      
      for (float i = 0.0; i < 8.0; ++i) {
        a += cos(i - d - a * uvFlow.x);
        d += sin(uvFlow.y * i + a);
      }
      d += t;
      
      vec2 uvScaled = uv * uFoldScale;
      vec2 holoUV = uvScaled * vec2(d, a);
      vec3 holoBase = vec3(
        cos(holoUV.x) * 0.5 + 0.5,
        cos(holoUV.y) * 0.5 + 0.5,
        cos(a + d) * 0.5 + 0.5
      );
      holoBase = cos(holoBase * cos(vec3(d, a, 2.5)) * 0.4 + 0.5) * 0.5 + 0.5;
      
      vec3 bgColor = uColors[0] * uColorBalance[0];
      float bgLuminance = dot(bgColor, vec3(0.299, 0.587, 0.114));
      bool isLightBg = bgLuminance > 0.5;
      
      float phase = fract((d + a) * 0.08);
      
      vec3 userColor = smoothSpectrum(phase, uColors, uColorBalance, blendSharpness);
      
      vec3 finalColor;
      float holoLum = dot(holoBase, vec3(0.33));
      if (isLightBg) {
        finalColor = mix(bgColor, userColor * holoBase, holoLum * uFoldIntensity * 0.7);
      } else {
        finalColor = bgColor + userColor * holoBase * uFoldIntensity * 0.6;
      }
      
      if (mouseInfluence > 0.01) {
        finalColor += userColor * mouseInfluence * 0.2;
      }
      
      if (uPulse > 0.01) {
        float pulseRing = sin(mouseDist * 12.0 - uPulse * 6.0) * 0.5 + 0.5;
        pulseRing *= smoothstep(0.7, 0.0, mouseDist) * smoothstep(0.0, 0.25, uPulse) * smoothstep(1.0, 0.4, uPulse);
        finalColor += userColor * pulseRing * 0.3;
      }
      
      if (uRimLight > 0.5) {
        float luminance = dot(finalColor, vec3(0.299, 0.587, 0.114));
        float rimThreshold = 1.0 - uRimFalloff * 0.08;
        float rim = smoothstep(rimThreshold, rimThreshold + 0.35, luminance);
        finalColor += uRimColor * rim * uRimIntensity * 0.8;
      }
      
      float depth = vDisplacement * uFoldIntensity;
      finalColor *= 0.97 + depth * 0.08;
      
      float grain = (snoise(vUv * 300.0 + uTime * 3.0) - 0.5) * uGrain * 0.8;
      finalColor += grain;
      
      float alpha = 1.0;
      if (uUseTextMask > 0.5) {
        alpha = texture2D(uTextMask, vUv).a;
      }

      gl_FragColor = vec4(clamp(finalColor, 0.0, 1.0), alpha);
    }
  `;

  // KOMOREBI - Neural network generated dark flowing patterns
  const KOMOREBI_FRAGMENT = `
    precision highp float;
    
    uniform float uTime;
    uniform vec2 uMouse;
    uniform vec2 uResolution;
    uniform vec2 uDisplacement;
    uniform float uPulse;
    uniform vec2 uLastClickPos;
    uniform vec3 uColors[7];
    uniform float uColorBalance[7];
    uniform float uSpeed;
    uniform float uFoldIntensity;
    uniform float uFoldScale;
    uniform float uFoldSpeed;
    uniform float uDirection;
    uniform float uGrain;
    uniform float uReactiveStrength;
    uniform float uMouseRadius;
    uniform float uScrollOffset;
    uniform float uRimLight;
    uniform float uRimIntensity;
    uniform vec3 uRimColor;
    uniform float uRimFalloff;
    uniform sampler2D uTextMask;
    uniform float uUseTextMask;
    uniform float uColorBlend;

    varying vec2 vUv;

    vec4 buf[8];
    
    float rand(vec2 c) {
      return fract(sin(dot(c, vec2(12.9898, 78.233))) * 43758.5453);
    }

    mat3 rgb2yiq = mat3(0.299, 0.587, 0.114, 0.596, -0.274, -0.322, 0.211, -0.523, 0.312);
    mat3 yiq2rgb = mat3(1.0, 0.956, 0.621, 1.0, -0.272, -0.647, 1.0, -1.106, 1.703);

    vec3 hueShiftRGB(vec3 col, float deg) {
      vec3 yiq = rgb2yiq * col;
      float rad = radians(deg);
      float cosh = cos(rad), sinh = sin(rad);
      vec3 yiqShift = vec3(yiq.x, yiq.y * cosh - yiq.z * sinh, yiq.y * sinh + yiq.z * cosh);
      return clamp(yiq2rgb * yiqShift, 0.0, 1.0);
    }

    vec4 sigmoid(vec4 x) { return 1.0 / (1.0 + exp(-x)); }

    vec4 cppn_fn(vec2 coordinate, float in0, float in1, float in2) {
      buf[6] = vec4(coordinate.x, coordinate.y, 0.3948333106474662 + in0, 0.36 + in1);
      buf[7] = vec4(0.14 + in2, sqrt(coordinate.x * coordinate.x + coordinate.y * coordinate.y), 0.0, 0.0);
      
      buf[0] = mat4(vec4(6.5404263, -3.6126034, 0.7590882, -1.13613), vec4(2.4582713, 3.1660357, 1.2219609, 0.06276096), vec4(-5.478085, -6.159632, 1.8701609, -4.7742867), vec4(6.039214, -5.542865, -0.90925294, 3.251348)) * buf[6] + mat4(vec4(0.8473259, -5.722911, 3.975766, 1.6522468), vec4(-0.24321538, 0.5839259, -1.7661959, -5.350116), vec4(0.0, 0.0, 0.0, 0.0), vec4(0.0, 0.0, 0.0, 0.0)) * buf[7] + vec4(0.21808943, 1.1243913, -1.7969975, 5.0294676);
      buf[1] = mat4(vec4(-3.3522482, -6.0612736, 0.55641043, -4.4719114), vec4(0.8631464, 1.7432913, 5.643898, 1.6106541), vec4(2.4941394, -3.5012043, 1.7184316, 6.357333), vec4(3.310376, 8.209261, 1.1355612, -1.165539)) * buf[6] + mat4(vec4(5.24046, -13.034365, 0.009859298, 15.870829), vec4(2.987511, 3.129433, -0.89023495, -1.6822904), vec4(0.0, 0.0, 0.0, 0.0), vec4(0.0, 0.0, 0.0, 0.0)) * buf[7] + vec4(-5.9457836, -6.573602, -0.8812491, 1.5436668);
      buf[0] = sigmoid(buf[0]); buf[1] = sigmoid(buf[1]);
      
      buf[2] = mat4(vec4(-15.219568, 8.095543, -2.429353, -1.9381982), vec4(-5.951362, 4.3115187, 2.6393783, 1.274315), vec4(-7.3145227, 6.7297835, 5.2473326, 5.9411426), vec4(5.0796127, 8.979051, -1.7278991, -1.158976)) * buf[6] + mat4(vec4(-11.967154, -11.608155, 6.1486754, 11.237008), vec4(2.124141, -6.263192, -1.7050359, -0.7021966), vec4(0.0, 0.0, 0.0, 0.0), vec4(0.0, 0.0, 0.0, 0.0)) * buf[7] + vec4(-4.17164, -3.2281182, -4.576417, -3.6401186);
      buf[3] = mat4(vec4(3.1832156, -13.738922, 1.879223, 3.233465), vec4(0.64300746, 12.768129, 1.9141049, 0.50990224), vec4(-0.049295485, 4.4807224, 1.4733979, 1.801449), vec4(5.0039253, 13.000481, 3.3991797, -4.5561905)) * buf[6] + mat4(vec4(-0.1285731, 7.720628, -3.1425676, 4.742367), vec4(0.6393625, 3.714393, -0.8108378, -0.39174938), vec4(0.0, 0.0, 0.0, 0.0), vec4(0.0, 0.0, 0.0, 0.0)) * buf[7] + vec4(-1.1811101, -21.621881, 0.7851888, 1.2329718);
      buf[2] = sigmoid(buf[2]); buf[3] = sigmoid(buf[3]);
      
      buf[4] = mat4(vec4(5.214916, -7.183024, 2.7228765, 2.6592617), vec4(-5.601878, -25.3591, 4.067988, 0.4602802), vec4(-10.57759, 24.286327, 21.102104, 37.546658), vec4(4.3024497, -1.9625226, 2.3458803, -1.372816)) * buf[0] + mat4(vec4(-17.6526, -10.507558, 2.2587414, 12.462782), vec4(6.265566, -502.75443, -12.642513, 0.9112289), vec4(-10.983244, 20.741234, -9.701768, -0.7635988), vec4(5.383626, 1.4819539, -4.1911616, -4.8444734)) * buf[1] + mat4(vec4(12.785233, -16.345072, -0.39901125, 1.7955981), vec4(-30.48365, -1.8345358, 1.4542528, -1.1118771), vec4(19.872723, -7.337935, -42.941723, -98.52709), vec4(8.337645, -2.7312303, -2.2927687, -36.142323)) * buf[2] + mat4(vec4(-16.298317, 3.5471997, -0.44300047, -9.444417), vec4(57.5077, -35.609753, 16.163465, -4.1534753), vec4(-0.07470326, -3.8656476, -7.0901804, 3.1523974), vec4(-12.559385, -7.077619, 1.490437, -0.8211543)) * buf[3] + vec4(-7.67914, 15.927437, 1.3207729, -1.6686112);
      buf[5] = mat4(vec4(-1.4109162, -0.372762, -3.770383, -21.367174), vec4(-6.2103205, -9.35908, 0.92529047, 8.82561), vec4(11.460242, -22.348068, 13.625772, -18.693201), vec4(-0.3429052, -3.9905605, -2.4626114, -0.45033523)) * buf[0] + mat4(vec4(7.3481627, -4.3661838, -6.3037653, -3.868115), vec4(1.5462853, 6.5488915, 1.9701879, -0.58291394), vec4(6.5858274, -2.2180402, 3.7127688, -1.3730392), vec4(-5.7973905, 10.134961, -2.3395722, -5.965605)) * buf[1] + mat4(vec4(-2.5132585, -6.6685553, -1.4029363, -0.16285264), vec4(-0.37908727, 0.53738135, 4.389061, -1.3024765), vec4(-0.70647055, 2.0111287, -5.1659346, -3.728635), vec4(-13.562562, 10.487719, -0.9173751, -2.6487076)) * buf[2] + mat4(vec4(-8.645013, 6.5546675, -6.3944063, -5.5933375), vec4(-0.57783127, -1.077275, 36.91025, 5.736769), vec4(14.283112, 3.7146652, 7.1452246, -4.5958776), vec4(2.7192075, 3.6021907, -4.366337, -2.3653464)) * buf[3] + vec4(-5.9000807, -4.329569, 1.2427121, 8.59503);
      buf[4] = sigmoid(buf[4]); buf[5] = sigmoid(buf[5]);
      
      buf[6] = mat4(vec4(-1.61102, 0.7970257, 1.4675229, 0.20917463), vec4(-28.793737, -7.1390953, 1.5025433, 4.656581), vec4(-10.94861, 39.66238, 0.74318546, -10.095605), vec4(-0.7229728, -1.5483948, 0.7301322, 2.1687684)) * buf[0] + mat4(vec4(3.2547753, 21.489103, -1.0194173, -3.3100595), vec4(-3.7316632, -3.3792162, -7.223193, -0.23685838), vec4(13.1804495, 0.7916005, 5.338587, 5.687114), vec4(-4.167605, -17.798311, -6.815736, -1.6451967)) * buf[1] + mat4(vec4(0.604885, -7.800309, -7.213122, -2.741014), vec4(-3.522382, -0.12359311, -0.5258442, 0.43852118), vec4(9.6752825, -22.853785, 2.062431, 0.099892326), vec4(-4.3196306, -17.730087, 2.5184598, 5.30267)) * buf[2] + mat4(vec4(-6.545563, -15.790176, -6.0438633, -5.415399), vec4(-43.591583, 28.551912, -16.00161, 18.84728), vec4(4.212382, 8.394307, 3.0958717, 8.657522), vec4(-5.0237565, -4.450633, -4.4768, -5.5010443)) * buf[3] + mat4(vec4(1.6985557, -67.05806, 6.897715, 1.9004834), vec4(1.8680354, 2.3915145, 2.5231109, 4.081538), vec4(11.158006, 1.7294737, 2.0738268, 7.386411), vec4(-4.256034, -306.24686, 8.258898, -17.132736)) * buf[4] + mat4(vec4(1.6889864, -4.5852966, 3.8534803, -6.3482175), vec4(1.3543309, -1.2640043, 9.932754, 2.9079645), vec4(-5.2770967, 0.07150358, -0.13962056, 3.3269649), vec4(28.34703, -4.918278, 6.1044083, 4.085355)) * buf[5] + vec4(6.6818056, 12.522166, -3.7075126, -4.104386);
      buf[7] = mat4(vec4(-8.265602, -4.7027016, 5.098234, 0.7509808), vec4(8.6507845, -17.15949, 16.51939, -8.884479), vec4(-4.036479, -2.3946867, -2.6055532, -1.9866527), vec4(-2.2167742, -1.8135649, -5.9759874, 4.8846445)) * buf[0] + mat4(vec4(6.7790847, 3.5076547, -2.8191125, -2.7028968), vec4(-5.743024, -0.27844876, 1.4958696, -5.0517144), vec4(13.122226, 15.735168, -2.9397483, -4.101023), vec4(-14.375265, -5.030483, -6.2599335, 2.9848232)) * buf[1] + mat4(vec4(4.0950394, -0.94011575, -5.674733, 4.755022), vec4(4.3809423, 4.8310084, 1.7425908, -3.437416), vec4(2.117492, 0.16342592, -104.56341, 16.949184), vec4(-5.22543, -2.994248, 3.8350096, -1.9364246)) * buf[2] + mat4(vec4(-5.900337, 1.7946124, -13.604192, -3.8060522), vec4(6.6583457, 31.911177, 25.164474, 91.81147), vec4(11.840538, 4.1503043, -0.7314397, 6.768467), vec4(-6.3967767, 4.034772, 6.1714606, -0.32874924)) * buf[3] + mat4(vec4(3.4992442, -196.91893, -8.923708, 2.8142626), vec4(3.4806502, -3.1846354, 5.1725626, 5.1804223), vec4(-2.4009497, 15.585794, 1.2863957, 2.0252278), vec4(-71.25271, -62.441242, -8.138444, 0.50670296)) * buf[4] + mat4(vec4(-12.291733, -11.176166, -7.3474145, 4.390294), vec4(10.805477, 5.6337385, -0.9385842, -4.7348723), vec4(-12.869276, -7.039391, 5.3029537, 7.5436664), vec4(1.4593618, 8.91898, 3.5101583, 5.840625)) * buf[5] + vec4(2.2415268, -6.705987, -0.98861027, -2.117676);
      buf[6] = sigmoid(buf[6]); buf[7] = sigmoid(buf[7]);
      
      buf[0] = mat4(vec4(1.6794263, 1.3817469, 2.9625452, 0.0), vec4(-1.8834411, -1.4806935, -3.5924516, 0.0), vec4(-1.3279216, -1.0918057, -2.3124623, 0.0), vec4(0.2662234, 0.23235129, 0.44178495, 0.0)) * buf[0] + mat4(vec4(-0.6299101, -0.5945583, -0.9125601, 0.0), vec4(0.17828953, 0.18300213, 0.18182953, 0.0), vec4(-2.96544, -2.5819945, -4.9001055, 0.0), vec4(1.4195864, 1.1868085, 2.5176322, 0.0)) * buf[1] + mat4(vec4(-1.2584374, -1.0552157, -2.1688404, 0.0), vec4(-0.7200217, -0.52666044, -1.438251, 0.0), vec4(0.15345335, 0.15196142, 0.272854, 0.0), vec4(0.945728, 0.8861938, 1.2766753, 0.0)) * buf[2] + mat4(vec4(-2.4218085, -1.968602, -4.35166, 0.0), vec4(-22.683098, -18.0544, -41.954372, 0.0), vec4(0.63792, 0.5470648, 1.1078634, 0.0), vec4(-1.5489894, -1.3075932, -2.6444845, 0.0)) * buf[3] + mat4(vec4(-0.49252132, -0.39877754, -0.91366625, 0.0), vec4(0.95609266, 0.7923952, 1.640221, 0.0), vec4(0.30616966, 0.15693925, 0.8639857, 0.0), vec4(1.1825981, 0.94504964, 2.176963, 0.0)) * buf[4] + mat4(vec4(0.35446745, 0.3293795, 0.59547555, 0.0), vec4(-0.58784515, -0.48177817, -1.0614829, 0.0), vec4(2.5271258, 1.9991658, 4.6846647, 0.0), vec4(0.13042648, 0.08864098, 0.30187556, 0.0)) * buf[5] + mat4(vec4(-1.7718065, -1.4033192, -3.3355875, 0.0), vec4(3.1664357, 2.638297, 5.378702, 0.0), vec4(-3.1724713, -2.6107926, -5.549295, 0.0), vec4(-2.851368, -2.249092, -5.3013067, 0.0)) * buf[6] + mat4(vec4(1.5203838, 1.2212278, 2.8404984, 0.0), vec4(1.5210563, 1.2651345, 2.683903, 0.0), vec4(2.9789467, 2.4364579, 5.2347264, 0.0), vec4(2.2270417, 1.8825914, 3.8028636, 0.0)) * buf[7] + vec4(-1.5468478, -3.6171484, 0.24762098, 0.0);
      buf[0] = sigmoid(buf[0]);
      
      return vec4(buf[0].x, buf[0].y, buf[0].z, 1.0);
    }

    void main() {
      float t = uTime * uSpeed * uFoldSpeed * 0.5;
      
      vec2 dirOffset = vec2(1.0, 1.0);
      if (uDirection == 1.0) dirOffset = vec2(0.0, 1.0);      // up
      else if (uDirection == 2.0) dirOffset = vec2(0.0, -1.0); // down
      else if (uDirection == 3.0) dirOffset = vec2(1.0, 0.0);
      else if (uDirection == 4.0) dirOffset = vec2(-1.0, 0.0);
      
      vec2 uv = vUv * 2.0 - 1.0;
      uv.y *= -1.0;
      
      uv += uDisplacement * 0.15;
      
      vec2 mouseUV = uMouse * 2.0 - 1.0;
      mouseUV.y *= -1.0;
      float mouseDist = distance(uv, mouseUV);
      float mouseInfluence = smoothstep(uMouseRadius * 2.0, 0.0, mouseDist) * uReactiveStrength;
      
      float warpAmount = uFoldIntensity;
      uv += warpAmount * vec2(sin(uv.y * 6.283 + t * 0.5 * dirOffset.y), cos(uv.x * 6.283 + t * 0.5 * dirOffset.x)) * 0.05;
      
      vec2 mouseOffset = (mouseUV - vec2(0.0)) * 0.5;
      uv += mouseInfluence * mouseOffset * 0.15;
      
      if (uPulse > 0.01) {
        vec2 clickUV = uLastClickPos * 2.0 - 1.0;
        clickUV.y *= -1.0;
        float clickDist = distance(uv, clickUV);
        float expandProgress = 1.0 - uPulse;
        float waveRadius = expandProgress * uMouseRadius * 4.0;
        float waveThickness = uMouseRadius * 0.8;
        float wave = smoothstep(waveRadius + waveThickness, waveRadius, clickDist) * 
                     smoothstep(waveRadius - waveThickness, waveRadius, clickDist);
        float clickInfluence = wave * uPulse;
        vec2 clickOffset = (clickUV - vec2(0.0)) * 0.4;
        uv += clickInfluence * clickOffset * 0.5;
      }
      
      uv *= uFoldScale;
      
      float scrollPhase = uScrollOffset * 0.15;
      
      uv.y += sin(scrollPhase) * 1.5 + sin(scrollPhase * 0.7) * 1.0;
      uv.x += cos(scrollPhase * 0.6) * 0.8 + sin(scrollPhase * 0.4) * 0.5;
      
      vec2 dirFlow = vec2(t * 0.15 * dirOffset.x, t * 0.15 * dirOffset.y);
      vec2 uvBase = uv + dirFlow;
      
      float tileSize = 4.0;
      float blendZone = 0.8;
      
      vec2 uvWrapped = mod(uvBase + 100.0, tileSize);
      vec2 edgeDist = min(uvWrapped, tileSize - uvWrapped);
      vec2 blendWeight = smoothstep(0.0, blendZone, edgeDist);
      float blend = blendWeight.x * blendWeight.y;
      
      vec2 uv1a = uvWrapped - tileSize * 0.5;
      vec2 uv1b = uv1a + vec2(tileSize * 0.5);
      
      vec2 uv2 = uv + vec2(sin(scrollPhase * 0.3 + 2.0), cos(scrollPhase * 0.5 + 1.0)) * 1.5;
      vec2 uv3 = uv + vec2(cos(scrollPhase * 0.4 + 3.0), sin(scrollPhase * 0.35 + 2.5)) * 2.0;
      
      vec4 cppn1a = cppn_fn(uv1a, 0.1 * sin(0.3 * t + scrollPhase * 0.1), 0.1 * sin(0.69 * t), 0.1 * sin(0.44 * t));
      vec4 cppn1b = cppn_fn(uv1b, 0.1 * sin(0.3 * t + scrollPhase * 0.1), 0.1 * sin(0.69 * t), 0.1 * sin(0.44 * t));
      vec4 cppn1 = mix(cppn1b, cppn1a, blend);
      vec4 cppn2 = cppn_fn(uv2 * 0.7, 0.1 * sin(0.4 * t + 1.0), 0.1 * sin(0.5 * t + scrollPhase * 0.08), 0.1 * sin(0.35 * t));
      vec4 cppn3 = cppn_fn(uv3 * 1.3, 0.1 * sin(0.25 * t + 2.0), 0.1 * sin(0.8 * t), 0.1 * sin(0.55 * t + scrollPhase * 0.12));
      
      vec3 cppnBlend = cppn1.rgb * 0.5 + cppn2.rgb * 0.3 + cppn3.rgb * 0.2;
      
      vec3 bgColor = uColors[0] * uColorBalance[0];
      vec3 color1 = uColors[1] * uColorBalance[1];
      vec3 color2 = uColors[2] * uColorBalance[2];
      vec3 color3 = uColors[3] * uColorBalance[3];
      
      float blendSharpness = uColorBlend == 1.0 ? 0.15 : (uColorBlend == 2.0 ? 0.05 : 0.35);
      
      float w1 = cppnBlend.r;
      float w2 = cppnBlend.g;
      float w3 = cppnBlend.b;
      
      float sharpPow = blendSharpness < 0.1 ? 8.0 : (blendSharpness < 0.25 ? 3.0 : 1.0);
      w1 = pow(w1, sharpPow);
      w2 = pow(w2, sharpPow);
      w3 = pow(w3, sharpPow);
      
      if (blendSharpness < 0.1) {
        w1 = w1 > 0.3 ? 1.0 : 0.0;
        w2 = w2 > 0.3 ? 1.0 : 0.0;
        w3 = w3 > 0.3 ? 1.0 : 0.0;
      }
      
      float lum = dot(cppnBlend, vec3(0.33));
      
      vec3 col = bgColor * (1.0 - lum * 0.8);
      col += color1 * w1 * 0.6;
      col += color2 * w2 * 0.5;
      col += color3 * w3 * 0.4;
      
      col = mix(col, mix(color1, color2, cppn2.r), cppn2.g * 0.3);
      
      col += color2 * pow(lum, 2.0) * 0.3;
      
      float vignette = 1.0 - length(vUv - 0.5) * 0.3;
      col *= vignette;
      
      if (uRimLight > 0.5) {
        float cppnLum = dot(cppnBlend, vec3(0.33));
        float rimThreshold = 0.3 - uRimFalloff * 0.05;
        float rim = smoothstep(rimThreshold, rimThreshold + 0.25, cppnLum);
        col += uRimColor * rim * uRimIntensity * 1.5;
      }
      
      col += (rand(gl_FragCoord.xy + t) - 0.5) * uGrain;
      
      float alpha = 1.0;
      if (uUseTextMask > 0.5) {
        alpha = texture2D(uTextMask, vUv).a;
      }
      
      gl_FragColor = vec4(clamp(col, 0.0, 1.0), alpha);
    }
  `;

  /* --------------------------------------------------
   *  Shader Registry
   * ------------------------------------------------*/
  const SHADERS = {
    aurora: { vertex: VERTEX_SHADER, fragment: AURORA_FRAGMENT },
    nebula: { vertex: VERTEX_SHADER, fragment: NEBULA_FRAGMENT },
    floral: { vertex: VERTEX_SHADER, fragment: FLORAL_FRAGMENT },
    oil: { vertex: VERTEX_SHADER, fragment: OIL_FRAGMENT },
    diva: { vertex: VERTEX_SHADER, fragment: DIVA_FRAGMENT },
    pixel: { vertex: VERTEX_SHADER, fragment: PIXEL_FRAGMENT },
    shimmer: { vertex: VERTEX_SHADER, fragment: SHIMMER_FRAGMENT },
    komorebi: { vertex: VERTEX_SHADER, fragment: KOMOREBI_FRAGMENT },
  };

  /* --------------------------------------------------
   *  Text Element Detection
   * ------------------------------------------------*/
  const TEXT_ELEMENTS = new Set([
    "h1",
    "h2",
    "h3",
    "h4",
    "h5",
    "h6",
    "p",
    "span",
    "a",
    "label",
    "button",
    "strong",
    "b",
    "em",
    "i",
    "u",
    "s",
    "strike",
    "del",
    "ins",
    "mark",
    "small",
    "sub",
    "sup",
    "code",
    "kbd",
    "samp",
    "var",
    "cite",
    "dfn",
    "abbr",
    "q",
    "time",
    "data",
    "blockquote",
    "pre",
    "address",
    "figcaption",
    "li",
    "dt",
    "dd",
    "th",
    "td",
    "caption",
    "legend",
    "option",
    "summary",
    "bdi",
    "bdo",
    "ruby",
    "rt",
    "rp",
    "wbr",
  ]);

  function isTextElement(element) {
    const tagName = element.tagName.toLowerCase();
    return TEXT_ELEMENTS.has(tagName);
  }

  function createTextMask(element, width, height) {
    const canvas = document.createElement("canvas");
    const ctx = canvas.getContext("2d");
    const dpr = Math.min(window.devicePixelRatio, 2);

    canvas.width = width * dpr;
    canvas.height = height * dpr;

    ctx.clearRect(0, 0, canvas.width, canvas.height);

    const styles = window.getComputedStyle(element);
    const fontSize = parseFloat(styles.fontSize);
    const fontFamily = styles.fontFamily;
    const fontWeight = styles.fontWeight;
    const fontStyle = styles.fontStyle;
    const lineHeight = styles.lineHeight;
    const paddingLeft = parseFloat(styles.paddingLeft) || 0;
    const paddingTop = parseFloat(styles.paddingTop) || 0;

    const scaledFontSize = fontSize * dpr;

    ctx.font = `${fontStyle} ${fontWeight} ${scaledFontSize}px ${fontFamily}`;
    ctx.fillStyle = "#ffffff";
    ctx.textBaseline = "top";
    ctx.textAlign = "left";

    const text = (element.textContent || "").trim();

    const x = paddingLeft * dpr;

    let y;
    const parsedLineHeight = parseFloat(lineHeight);
    if (!isNaN(parsedLineHeight)) {
      y = ((parsedLineHeight - fontSize) / 2) * dpr + paddingTop * dpr;
    } else {
      y = paddingTop * dpr;
    }

    ctx.fillText(text, x, y);

    return canvas;
  }

  /* --------------------------------------------------
   *  Shared Renderer Architecture
   * ------------------------------------------------*/
  class SpectraGLRenderer {
    constructor() {
      this.effects = new Map();
      this.targetElements = new Map();
      this.displayCanvases = new Map();
      this.textMasks = new Map();

      this.sharedRenderer = null;
      this.sharedPixelRatio = Math.min(window.devicePixelRatio, 2);
      this.lastRenderedWidth = 0;
      this.lastRenderedHeight = 0;

      this.mouse = new THREE.Vector2(0.5, 0.5);
      this.targetMouse = new THREE.Vector2(0.5, 0.5);
      this.scrollY = window.scrollY;
      this.time = 0;

      this.onMouseMove = this.onMouseMove.bind(this);
      this.onMouseDown = this.onMouseDown.bind(this);
      this.onTouchMove = this.onTouchMove.bind(this);
      this.onTouchStart = this.onTouchStart.bind(this);
      this.onScroll = this.onScroll.bind(this);
      this.onVisibilityChange = this.onVisibilityChange.bind(this);

      document.addEventListener("mousemove", this.onMouseMove);
      document.addEventListener("mousedown", this.onMouseDown);
      document.addEventListener("touchstart", this.onTouchStart, {
        passive: true,
      });
      document.addEventListener("touchmove", this.onTouchMove, {
        passive: true,
      });
      window.addEventListener("scroll", this.onScroll, { passive: true });
      window.addEventListener("wheel", this.onWheel.bind(this), {
        passive: true,
      });

      document.addEventListener("visibilitychange", this.onVisibilityChange);

      this.initSharedRenderer();
    }

    initSharedRenderer() {
      const contextType = WebGLSupport.getSupported();
      if (!contextType) return false;

      const canvas = document.createElement("canvas");
      const contextAttributes = {
        alpha: true,
        antialias: !isSafari(),
        powerPreference: "high-performance",
        preserveDrawingBuffer: !isSafari(),
      };

      let gl;
      if (contextType === "webgl2") {
        gl = canvas.getContext("webgl2", contextAttributes);
      } else {
        gl =
          canvas.getContext("webgl", contextAttributes) ||
          canvas.getContext("experimental-webgl", contextAttributes);
      }

      if (!gl) return false;

      this.sharedRenderer = new THREE.WebGLRenderer({
        canvas: canvas,
        context: gl,
        antialias: !isSafari(),
        alpha: true,
        powerPreference: "high-performance",
      });

      this.sharedRenderer.setPixelRatio(this.sharedPixelRatio);
      this.sharedRenderer.setClearColor(0x000000, 0);
      this.sharedRenderer.autoClear = true;

      return true;
    }

    createMesh(options) {
      const shaderConfig = SHADERS[options.mode];
      if (!shaderConfig) {
        console.error(`spectraGL: Unknown mode "${options.mode}"`);
        return null;
      }

      let segments = Math.min(options.meshDetail, 128);
      if (isSafari()) {
        const heavyModes = ["nebula", "oil", "floral"];
        if (heavyModes.includes(options.mode)) {
          segments = Math.max(8, Math.floor(segments * 0.5));
        }
      }
      const geometry = new THREE.PlaneGeometry(2, 2, segments, segments);

      const colors = [...options.colors];
      while (colors.length < 7) {
        colors.push(colors[colors.length - 1] || "#ffffff");
      }

      const colorBalance = options.colorBalance || [];
      const balanceArray = [];
      for (let i = 0; i < 7; i++) {
        balanceArray.push(
          colorBalance[i] !== undefined ? colorBalance[i] : 1.0
        );
      }

      const blendModes = { smooth: 0, sharp: 1, stepped: 2 };
      const colorBlendValue = blendModes[options.colorBlend] || 0;

      const directionModes = { auto: 0, up: 1, down: 2, left: 3, right: 4 };
      const directionValue = directionModes[options.direction] || 0;

      const material = new THREE.ShaderMaterial({
        uniforms: {
          uTime: { value: 0 },
          uMouse: { value: new THREE.Vector2(0.5, 0.5) },
          uMouseVelocity: { value: new THREE.Vector2(0, 0) },
          uDisplacement: { value: new THREE.Vector2(0, 0) },
          uClickOffset: { value: 0 },
          uLastClickPos: { value: new THREE.Vector2(0.5, 0.5) },
          uPulse: { value: 0 },
          uResolution: { value: new THREE.Vector2(1, 1) },
          uColors: { value: colors.slice(0, 7).map((c) => new THREE.Color(c)) },
          uColorBlend: { value: colorBlendValue },
          uColorBalance: { value: balanceArray },
          uDirection: { value: directionValue },
          uSpeed: { value: options.speed },
          uTextMask: { value: null },
          uUseTextMask: { value: 0.0 },
          uFoldIntensity: { value: options.foldIntensity },
          uFoldScale: { value: options.foldScale },
          uFoldSpeed: { value: options.foldSpeed },
          uRimLight: { value: options.rimLight ? 1.0 : 0.0 },
          uRimIntensity: { value: options.rimIntensity },
          uRimColor: { value: new THREE.Color(options.rimColor) },
          uRimFalloff: { value: options.rimFalloff },
          uGrain: { value: options.grain },
          uReactiveStrength: {
            value: options.reactive ? options.reactiveStrength : 0,
          },
          uMouseRadius: { value: options.mouseRadius },
          uScrollOffset: { value: 0 },
          uScrollVelocity: { value: 0 },
        },
        vertexShader: shaderConfig.vertex,
        fragmentShader: shaderConfig.fragment,
        transparent: true,
        side: THREE.DoubleSide,
      });

      return new THREE.Mesh(geometry, material);
    }

    addEffect(instanceId, targetElement, options) {
      if (!this.sharedRenderer) {
        this.applyFallback(targetElement, options);
        return false;
      }

      const isText = isTextElement(targetElement);
      const useTextMask = isText && !options.border?.enabled;

      if (useTextMask) {
        options._isTextElement = true;
      }

      const mesh = this.createMesh(options);
      if (!mesh) {
        return false;
      }

      const scene = new THREE.Scene();
      const camera = new THREE.OrthographicCamera(-1, 1, 1, -1, -100, 100);
      camera.position.z = 10;
      scene.add(mesh);

      const stacking = getStackingProperties(targetElement);

      const displayCanvas = document.createElement("canvas");
      displayCanvas.setAttribute("data-spectra-id", instanceId);
      const ctx = displayCanvas.getContext("2d", { alpha: true });

      if (stacking.isFixedContext) {
        document.body.appendChild(displayCanvas);
      } else {
        targetElement.parentNode.insertBefore(displayCanvas, targetElement);
      }

      this.displayCanvases.set(instanceId, {
        canvas: displayCanvas,
        ctx: ctx,
        lastWidth: 0,
        lastHeight: 0,
      });

      this.effects.set(instanceId, {
        mesh,
        scene,
        camera,
        element: targetElement,
        options,
        inView: true,
        observer: null,
        originalVisibility: targetElement.style.visibility,
        currentMouse: new THREE.Vector2(0.5, 0.5),
        prevMouse: new THREE.Vector2(0.5, 0.5),
        mouseVelocity: new THREE.Vector2(0, 0),
        displacement: new THREE.Vector2(0, 0),
        pulse: 0,
        clickOffset: 0,
        clickOffsetTarget: 0,
        lastClickPos: new THREE.Vector2(0.5, 0.5),
        scrollOffset: 0,
        scrollVelocity: 0,
        stacking,
        useTextMask,
      });

      if (useTextMask) {
        this.updateTextMask(instanceId, targetElement, mesh);
      }

      this.targetElements.set(instanceId, targetElement);

      if ("IntersectionObserver" in window) {
        const observer = new IntersectionObserver(
          (entries) => {
            const effectData = this.effects.get(instanceId);
            if (effectData) {
              effectData.inView = entries[0].isIntersecting;
            }
          },
          { root: null, threshold: 0, rootMargin: "200px" }
        );
        observer.observe(targetElement);
        this.effects.get(instanceId).observer = observer;
      }

      if (!options.border || !options.border.enabled) {
        if (!useTextMask) {
          const parent = targetElement.parentNode;
          if (parent && parent !== document.body) {
            const parentStyle = window.getComputedStyle(parent);
            const hasTransform =
              parentStyle.transform && parentStyle.transform !== "none";
            const hasFilter =
              parentStyle.filter && parentStyle.filter !== "none";
            if (
              parentStyle.isolation !== "isolate" &&
              parentStyle.zIndex === "auto" &&
              !hasTransform &&
              !hasFilter &&
              parentStyle.opacity === "1"
            ) {
              parent.style.isolation = "isolate";
            }
          }

          setTimeout(() => {
            targetElement.style.visibility = "hidden";
          }, 16);
        } else {
          setTimeout(() => {
            targetElement.style.color = "transparent";
          }, 16);
        }
      }

      return true;
    }

    updateTextMask(instanceId, element, mesh) {
      const rect = element.getBoundingClientRect();
      if (rect.width <= 0 || rect.height <= 0) {
        const effectData = this.effects.get(instanceId);
        if (effectData) effectData.needsTextMask = true;
        return;
      }

      const maskCanvas = createTextMask(element, rect.width, rect.height);

      const texture = new THREE.CanvasTexture(maskCanvas);
      texture.needsUpdate = true;
      texture.minFilter = THREE.LinearFilter;
      texture.magFilter = THREE.LinearFilter;
      texture.format = THREE.RGBAFormat;
      texture.premultiplyAlpha = false;

      const oldTexture = this.textMasks.get(instanceId);
      if (oldTexture) oldTexture.dispose();
      this.textMasks.set(instanceId, texture);

      if (mesh && mesh.material && mesh.material.uniforms) {
        mesh.material.uniforms.uTextMask.value = texture;
        mesh.material.uniforms.uUseTextMask.value = 1.0;
        mesh.material.needsUpdate = true;
      }
    }

    applyFallback(element, options) {
      const colors = options.colors.slice(0, 3);
      const gradient = `linear-gradient(135deg, ${colors.join(", ")})`;
      element.style.background = gradient;
      element.setAttribute("data-spectra-fallback", "true");
    }

    removeEffect(instanceId, keepTargetHidden = false) {
      const effectData = this.effects.get(instanceId);

      if (effectData) {
        if (!keepTargetHidden) {
          effectData.element.style.visibility =
            effectData.originalVisibility || "";
        }

        const displayData = this.displayCanvases.get(instanceId);
        if (displayData && displayData.canvas) {
          displayData.canvas.remove();
        }
        this.displayCanvases.delete(instanceId);

        if (effectData.mesh) {
          if (effectData.mesh.geometry) effectData.mesh.geometry.dispose();
          if (effectData.mesh.material) effectData.mesh.material.dispose();
        }

        const maskTexture = this.textMasks.get(instanceId);
        if (maskTexture) {
          maskTexture.dispose();
          this.textMasks.delete(instanceId);
        }

        if (effectData.observer) effectData.observer.disconnect();

        this.effects.delete(instanceId);
        this.targetElements.delete(instanceId);
      }
    }

    onMouseMove(event) {
      for (const [instanceId, effectData] of this.effects) {
        if (!effectData.options.reactive) continue;

        const rect = effectData.element.getBoundingClientRect();
        const isOver =
          event.clientX >= rect.left &&
          event.clientX <= rect.right &&
          event.clientY >= rect.top &&
          event.clientY <= rect.bottom;

        if (isOver) {
          const x = (event.clientX - rect.left) / rect.width;
          const y = 1.0 - (event.clientY - rect.top) / rect.height;
          effectData.currentMouse.set(x, y);
        }
      }
    }

    onMouseDown(event) {
      for (const [instanceId, effectData] of this.effects) {
        const rect = effectData.element.getBoundingClientRect();
        const isOver =
          event.clientX >= rect.left &&
          event.clientX <= rect.right &&
          event.clientY >= rect.top &&
          event.clientY <= rect.bottom;

        if (isOver && effectData.options.reactive) {
          effectData.pulse = 1.0;
          const x = (event.clientX - rect.left) / rect.width;
          const y = 1.0 - (event.clientY - rect.top) / rect.height;
          effectData.lastClickPos = new THREE.Vector2(x, y);

          if (effectData.options.mode === "aurora") {
            effectData.clickOffsetTarget =
              (effectData.clickOffsetTarget || 0) + 0.6;
          }
        }
      }
    }

    onTouchStart(event) {
      if (event.touches.length > 0) {
        const touch = event.touches[0];
        for (const [instanceId, effectData] of this.effects) {
          const rect = effectData.element.getBoundingClientRect();
          const isOver =
            touch.clientX >= rect.left &&
            touch.clientX <= rect.right &&
            touch.clientY >= rect.top &&
            touch.clientY <= rect.bottom;

          if (isOver) {
            effectData.lastTouchY = touch.clientY;

            if (effectData.options.reactive) {
              const x = (touch.clientX - rect.left) / rect.width;
              const y = 1.0 - (touch.clientY - rect.top) / rect.height;
              effectData.currentMouse.set(x, y);
              effectData.pulse = 1.0;
              if (effectData.options.mode === "aurora") {
                effectData.clickOffsetTarget =
                  (effectData.clickOffsetTarget || 0) + 0.6;
                effectData.lastClickPos = new THREE.Vector2(x, y);
              }
            }
          }
        }
      }
    }

    onTouchMove(event) {
      if (event.touches.length > 0) {
        const touch = event.touches[0];
        for (const [instanceId, effectData] of this.effects) {
          const rect = effectData.element.getBoundingClientRect();
          const isOver =
            touch.clientX >= rect.left &&
            touch.clientX <= rect.right &&
            touch.clientY >= rect.top &&
            touch.clientY <= rect.bottom;

          if (isOver) {
            if (
              effectData.options.scrollReactive &&
              effectData.lastTouchY !== undefined
            ) {
              const deltaY = effectData.lastTouchY - touch.clientY;
              const strength = effectData.options.scrollStrength || 0.2;
              effectData.scrollVelocity -= deltaY * 0.003 * strength;
              effectData.lastTouchY = touch.clientY;
            }

            if (effectData.options.reactive) {
              const x = (touch.clientX - rect.left) / rect.width;
              const y = 1.0 - (touch.clientY - rect.top) / rect.height;
              effectData.currentMouse.set(x, y);
            }
          }
        }
      }
    }

    onScroll() {
      this.scrollY = window.scrollY;
    }

    onWheel(event) {
      for (const [instanceId, effectData] of this.effects) {
        if (!effectData.options.scrollReactive) continue;

        const rect = effectData.element.getBoundingClientRect();
        const isOver =
          event.clientX >= rect.left &&
          event.clientX <= rect.right &&
          event.clientY >= rect.top &&
          event.clientY <= rect.bottom;

        if (isOver) {
          const strength = effectData.options.scrollStrength || 0.2;
          effectData.scrollVelocity -= event.deltaY * 0.0008 * strength;
        }
      }
    }

    onVisibilityChange() {
      isPageVisible = !document.hidden;
    }

    animate() {
      if (!isAnimating) return;
      if (!this.sharedRenderer) {
        animationFrameId = requestAnimationFrame(() => this.animate());
        return;
      }

      if (!isPageVisible) {
        animationFrameId = requestAnimationFrame(() => this.animate());
        return;
      }

      const currentTime = performance.now();
      const deltaTime = currentTime - lastFrameTime;

      if (deltaTime < targetFrameTime) {
        animationFrameId = requestAnimationFrame(() => this.animate());
        return;
      }

      lastFrameTime = currentTime - (deltaTime % targetFrameTime);
      this.time += Math.min(deltaTime / 1000, 0.1);

      for (const [instanceId, effectData] of this.effects) {
        if (!effectData.inView) continue;

        const { mesh, scene, camera, options, element } = effectData;
        const displayData = this.displayCanvases.get(instanceId);
        if (!displayData) continue;

        const { canvas: displayCanvas, ctx } = displayData;

        if (
          !effectData.cachedRect ||
          effectData.rectCacheTime < currentTime - 100
        ) {
          effectData.cachedRect = element.getBoundingClientRect();
          effectData.rectCacheTime = currentTime;
        }
        const rect = effectData.cachedRect;

        if (rect.width <= 1 || rect.height <= 1) continue;

        if (effectData.needsTextMask && effectData.useTextMask) {
          this.updateTextMask(instanceId, element, mesh);
          effectData.needsTextMask = false;
        }

        const velocityX = effectData.currentMouse.x - effectData.prevMouse.x;
        const velocityY = effectData.currentMouse.y - effectData.prevMouse.y;
        effectData.mouseVelocity.set(velocityX, velocityY);
        effectData.prevMouse.copy(effectData.currentMouse);

        if (options.reactive) {
          const displacementStrength =
            (options.displacementStrength !== undefined
              ? options.displacementStrength
              : 0.3) * 26.67;
          effectData.displacement.x += velocityX * displacementStrength;
          effectData.displacement.y += velocityY * displacementStrength;
          effectData.displacement.x *= 0.992;
          effectData.displacement.y *= 0.992;
        }

        if (effectData.pulse > 0) {
          let decayRate = 0.96;
          if (effectData.options.mode === "komorebi") decayRate = 0.99;

          effectData.pulse *= decayRate;
          if (effectData.pulse < 0.01) effectData.pulse = 0;
        }

        if (effectData.options.mode === "aurora") {
          effectData.clickOffset +=
            (effectData.clickOffsetTarget - effectData.clickOffset) * 0.08;
        }

        const scrollStrength = options.scrollStrength || 0.2;
        const decay = 0.88 + scrollStrength * 0.06;
        effectData.scrollOffset += effectData.scrollVelocity;
        effectData.scrollVelocity *= decay;

        const uniforms = mesh.material.uniforms;
        uniforms.uTime.value = this.time;
        uniforms.uMouse.value.lerp(effectData.currentMouse, 0.1);

        uniforms.uMouseVelocity?.value.copy(effectData.mouseVelocity);
        uniforms.uDisplacement?.value.copy(effectData.displacement);

        if (uniforms.uPulse) uniforms.uPulse.value = effectData.pulse;
        if (uniforms.uClickOffset)
          uniforms.uClickOffset.value = effectData.clickOffset || 0;
        if (uniforms.uLastClickPos) {
          uniforms.uLastClickPos.value =
            effectData.lastClickPos || new THREE.Vector2(0.5, 0.5);
        }
        if (uniforms.uScrollOffset)
          uniforms.uScrollOffset.value = effectData.scrollOffset;
        if (uniforms.uScrollVelocity) {
          uniforms.uScrollVelocity.value =
            Math.abs(effectData.scrollVelocity) * 100;
        }

        const stacking = effectData.stacking;

        if (
          !effectData.cachedStyle ||
          effectData.styleCacheTime < currentTime - 500
        ) {
          effectData.cachedStyle = window.getComputedStyle(element);
          effectData.styleCacheTime = currentTime;
        }
        const computedStyle = effectData.cachedStyle;

        let renderWidth, renderHeight;

        if (options.border && options.border.enabled) {
          const borderWidth = options.border.width || 2;
          renderWidth = rect.width + borderWidth * 2;
          renderHeight = rect.height + borderWidth * 2;
        } else {
          renderWidth = rect.width;
          renderHeight = rect.height;
        }

        const pxWidth = Math.floor(renderWidth * this.sharedPixelRatio);
        const pxHeight = Math.floor(renderHeight * this.sharedPixelRatio);

        if (
          pxWidth !== this.lastRenderedWidth ||
          pxHeight !== this.lastRenderedHeight
        ) {
          this.sharedRenderer.setSize(renderWidth, renderHeight);
          this.lastRenderedWidth = pxWidth;
          this.lastRenderedHeight = pxHeight;
        }

        const sizeChanged =
          displayData.lastWidth !== pxWidth ||
          displayData.lastHeight !== pxHeight;
        if (sizeChanged) {
          displayCanvas.width = pxWidth;
          displayCanvas.height = pxHeight;
          displayData.lastWidth = pxWidth;
          displayData.lastHeight = pxHeight;
        }

        uniforms.uResolution.value.set(pxWidth, pxHeight);

        this.sharedRenderer.render(scene, camera);

        if (sizeChanged) {
          ctx.clearRect(0, 0, pxWidth, pxHeight);
        }
        ctx.drawImage(this.sharedRenderer.domElement, 0, 0, pxWidth, pxHeight);

        displayCanvas.style.position = stacking.position;
        displayCanvas.style.display = "block";
        displayCanvas.style.visibility = "visible";
        displayCanvas.style.pointerEvents = "none";
        displayCanvas.style.willChange = "transform";
        displayCanvas.style.width = `${renderWidth}px`;
        displayCanvas.style.height = `${renderHeight}px`;

        if (stacking.position === "fixed") {
          displayCanvas.style.top = `${rect.top}px`;
          displayCanvas.style.left = `${rect.left}px`;
          displayCanvas.style.zIndex = String(stacking.zIndex + 1);
        } else {
          displayCanvas.style.top = `${element.offsetTop}px`;
          displayCanvas.style.left = `${element.offsetLeft}px`;
          const targetStyle = window.getComputedStyle(element);
          displayCanvas.style.zIndex =
            targetStyle.zIndex === "auto" ? "0" : targetStyle.zIndex;
        }

        if (options.border && options.border.enabled) {
          const borderWidth = options.border.width || 2;

          const parent = element.parentNode;
          parent.style.overflow = "visible";
          parent.style.position = "relative";
          parent.style.zIndex = "0";

          displayCanvas.style.top = `-${borderWidth}px`;
          displayCanvas.style.left = `-${borderWidth}px`;
          displayCanvas.style.zIndex = "-1";

          const parentStyle = window.getComputedStyle(parent);
          let innerRadius = parseFloat(parentStyle.borderRadius) || 0;
          if (options.border.radius) {
            innerRadius = options.border.radius;
          }
          const outerRadius = innerRadius + borderWidth;
          displayCanvas.style.borderRadius = `${outerRadius}px`;
          displayCanvas.style.overflow = "hidden";

          const bw = borderWidth;
          const w = renderWidth;
          const h = renderHeight;
          const iw = rect.width;
          const ih = rect.height;
          const ir = innerRadius;
          const or = outerRadius;

          const svg = `<svg xmlns="http://www.w3.org/2000/svg" width="${w}" height="${h}"><path fill-rule="evenodd" fill="white" d="M${or},0 H${
            w - or
          } A${or},${or} 0 0 1 ${w},${or} V${h - or} A${or},${or} 0 0 1 ${
            w - or
          },${h} H${or} A${or},${or} 0 0 1 0,${
            h - or
          } V${or} A${or},${or} 0 0 1 ${or},0 Z M${
            bw + ir
          },${bw} A${ir},${ir} 0 0 0 ${bw},${bw + ir} V${
            bw + ih - ir
          } A${ir},${ir} 0 0 0 ${bw + ir},${bw + ih} H${
            bw + iw - ir
          } A${ir},${ir} 0 0 0 ${bw + iw},${bw + ih - ir} V${
            bw + ir
          } A${ir},${ir} 0 0 0 ${bw + iw - ir},${bw} Z"/></svg>`;
          const maskUri = `url("data:image/svg+xml,${encodeURIComponent(
            svg
          )}")`;
          displayCanvas.style.maskImage = maskUri;
          displayCanvas.style.webkitMaskImage = maskUri;
          displayCanvas.style.maskSize = "100% 100%";
          displayCanvas.style.webkitMaskSize = "100% 100%";
        } else {
          if (displayCanvas.style.borderRadius !== computedStyle.borderRadius) {
            displayCanvas.style.borderRadius = computedStyle.borderRadius;
          }
          displayCanvas.style.overflow = "hidden";
          displayCanvas.style.maskImage = "none";
          displayCanvas.style.webkitMaskImage = "none";
        }
      }

      animationFrameId = requestAnimationFrame(() => this.animate());
    }

    startAnimation() {
      if (!isAnimating) {
        isAnimating = true;
        this.animate();
      }
    }

    stopAnimation() {
      if (isAnimating) {
        isAnimating = false;
        if (animationFrameId) {
          cancelAnimationFrame(animationFrameId);
          animationFrameId = null;
        }
      }
    }

    dispose() {
      this.stopAnimation();

      for (const [instanceId] of this.effects) {
        this.removeEffect(instanceId);
      }

      document.removeEventListener("mousemove", this.onMouseMove);
      document.removeEventListener("mousedown", this.onMouseDown);
      document.removeEventListener("touchstart", this.onTouchStart);
      document.removeEventListener("touchmove", this.onTouchMove);
      window.removeEventListener("scroll", this.onScroll);
      document.removeEventListener("visibilitychange", this.onVisibilityChange);

      if (this.sharedRenderer) {
        this.sharedRenderer.dispose();
        this.sharedRenderer = null;
      }

      this.effects.clear();
      this.targetElements.clear();
      this.displayCanvases.clear();
    }
  }

  /* --------------------------------------------------
   *  DOM Analysis Helper
   * ------------------------------------------------*/
  function getStackingProperties(element) {
    let zIndex = 0;
    let isFixed = false;
    let el = element;

    while (el && el !== document.body) {
      const style = window.getComputedStyle(el);
      const pos = style.position;
      const z = parseInt(style.zIndex, 10);

      if (pos === "fixed") isFixed = true;
      if (pos !== "static" && !isNaN(z)) zIndex = Math.max(zIndex, z);

      el = el.parentElement;
    }

    const style = window.getComputedStyle(element);
    const elementZ = parseInt(style.zIndex, 10);
    if (style.position !== "static" && !isNaN(elementZ)) {
      zIndex = Math.max(zIndex, elementZ);
    }

    return {
      position: isFixed ? "fixed" : "absolute",
      zIndex,
      isFixedContext: isFixed,
    };
  }

  /* --------------------------------------------------
   *  Instance Management
   * ------------------------------------------------*/
  class SpectraGLInstance {
    constructor(options, elements) {
      this.id = `spectra-${++instanceCounter}`;
      this.options = options;
      this.elements = elements;
      this.initialized = false;
      this.paused = false;
      this.supported = WebGLSupport.getSupported();
      this.fallbackApplied = false;
      this.helperGUI = null;
    }

    init() {
      if (this.initialized) return;

      if (!globalRenderer) {
        globalRenderer = new SpectraGLRenderer();
        globalRenderer.startAnimation();
        setupGlobalResizeObserver();
      }

      for (let i = 0; i < this.elements.length; i++) {
        const element = this.elements[i];
        const elementId = element.id || `el-${i}`;
        const instanceId = `${this.id}-${elementId}`;

        const success = globalRenderer.addEffect(
          instanceId,
          element,
          this.options
        );
        if (!success) {
          this.fallbackApplied = true;
        }
      }

      this.initialized = true;
      activeInstances.set(this.id, this);

      if (this.options.helper) {
        loadLilGui()
          .then(() => {
            const instanceIndex = helperGUIs.length;
            this.helperGUI = createHelperGUI(this, instanceIndex);
          })
          .catch((err) => {
            console.error("Failed to load helper GUI:", err);
          });
      }

      if (this.options.on && this.options.on.init) {
        this.options.on.init(this);
      }
    }

    destroy() {
      if (!this.initialized) return;

      if (this.helperGUI) {
        destroyHelperGUI(this);
        this.helperGUI = null;
      }

      for (let i = 0; i < this.elements.length; i++) {
        const element = this.elements[i];
        const elementId = element.id || `el-${i}`;
        const instanceId = `${this.id}-${elementId}`;
        globalRenderer.removeEffect(instanceId);
      }

      activeInstances.delete(this.id);

      if (activeInstances.size === 0 && globalRenderer) {
        globalRenderer.dispose();
        globalRenderer = null;
        cleanupGlobalResizeObserver();
      }

      this.initialized = false;

      if (this.options.on && this.options.on.destroy) {
        this.options.on.destroy(this);
      }
    }

    pause() {
      this.paused = true;
    }

    play() {
      this.paused = false;
    }

    updateOptions(newOptions) {
      const oldOptions = { ...this.options };
      this.options = { ...this.options, ...newOptions };

      const reinitKeys = ["target", "mode", "meshDetail"];
      const needsReinit = reinitKeys.some(
        (key) =>
          newOptions[key] !== undefined && newOptions[key] !== oldOptions[key]
      );

      if (needsReinit) {
        this.destroy();
        setTimeout(() => this.init(), 50);
      } else {
        this.updateLiveUniforms(newOptions);
      }
    }

    updateLiveUniforms(newOptions) {
      if (!globalRenderer) return;

      for (let i = 0; i < this.elements.length; i++) {
        const element = this.elements[i];
        const elementId = element.id || `el-${i}`;
        const instanceId = `${this.id}-${elementId}`;
        const effectData = globalRenderer.effects.get(instanceId);

        if (effectData && effectData.mesh.material.uniforms) {
          const u = effectData.mesh.material.uniforms;

          if (newOptions.speed !== undefined) u.uSpeed.value = newOptions.speed;
          if (newOptions.foldIntensity !== undefined)
            u.uFoldIntensity.value = newOptions.foldIntensity;
          if (newOptions.foldScale !== undefined)
            u.uFoldScale.value = newOptions.foldScale;
          if (newOptions.foldSpeed !== undefined)
            u.uFoldSpeed.value = newOptions.foldSpeed;
          if (newOptions.rimIntensity !== undefined)
            u.uRimIntensity.value = newOptions.rimIntensity;
          if (newOptions.rimColor !== undefined && u.uRimColor.value)
            u.uRimColor.value.set(newOptions.rimColor);
          if (newOptions.rimFalloff !== undefined)
            u.uRimFalloff.value = newOptions.rimFalloff;
          if (newOptions.grain !== undefined) u.uGrain.value = newOptions.grain;
          if (newOptions.reactiveStrength !== undefined)
            u.uReactiveStrength.value = newOptions.reactiveStrength;
          if (newOptions.displacementStrength !== undefined)
            effectData.options.displacementStrength =
              newOptions.displacementStrength;
          if (newOptions.mouseRadius !== undefined)
            u.uMouseRadius.value = newOptions.mouseRadius;
          if (newOptions.colors !== undefined && u.uColors.value) {
            const sourceColors = newOptions.colors;
            const targetColors = u.uColors.value;
            for (let i = 0; i < 7; i++) {
              if (targetColors[i]) {
                const colorVal =
                  sourceColors[i] !== undefined
                    ? sourceColors[i]
                    : sourceColors.length > 0
                    ? sourceColors[sourceColors.length - 1]
                    : "#ffffff";
                targetColors[i].set(colorVal);
              }
            }
          }
          if (newOptions.colorBlend !== undefined) {
            const blendModes = { smooth: 0, sharp: 1, stepped: 2 };
            u.uColorBlend.value = blendModes[newOptions.colorBlend] || 0;
          }
          if (newOptions.colorBalance !== undefined) {
            const balance = newOptions.colorBalance || [];
            const balanceArray = [];
            for (let i = 0; i < 7; i++) {
              balanceArray.push(balance[i] !== undefined ? balance[i] : 1.0);
            }
            u.uColorBalance.value = balanceArray;
          }
          if (newOptions.direction !== undefined) {
            const directionModes = {
              auto: 0,
              up: 1,
              down: 2,
              left: 3,
              right: 4,
            };
            u.uDirection.value = directionModes[newOptions.direction] || 0;
          }
          if (newOptions.rimLight !== undefined) {
            u.uRimLight.value = newOptions.rimLight ? 1.0 : 0.0;
          }

          effectData.options = { ...effectData.options, ...newOptions };
        }
      }
    }
  }

  /* --------------------------------------------------
   *  Global Resize Observer
   * ------------------------------------------------*/
  function setupGlobalResizeObserver() {
    if (resizeObserver) return;

    let lastWidth = window.innerWidth;
    let lastHeight = window.innerHeight;

    const handleResize = () => {
      if (resizeDebounceTimer) clearTimeout(resizeDebounceTimer);

      resizeDebounceTimer = setTimeout(() => {
        const widthChanged = Math.abs(window.innerWidth - lastWidth) > 10;
        const heightChanged = Math.abs(window.innerHeight - lastHeight) > 100;

        if (widthChanged || heightChanged) {
          for (const [, instance] of activeInstances) {
            if (instance && instance.initialized) {
              instance.destroy();
              setTimeout(() => instance.init(), 50);
            }
          }
          lastWidth = window.innerWidth;
          lastHeight = window.innerHeight;
        }
      }, 250);
    };

    window.addEventListener("resize", handleResize);
    resizeObserver = { handleResize };
  }

  function cleanupGlobalResizeObserver() {
    if (resizeObserver) {
      window.removeEventListener("resize", resizeObserver.handleResize);
      resizeObserver = null;
    }
    if (resizeDebounceTimer) {
      clearTimeout(resizeDebounceTimer);
      resizeDebounceTimer = null;
    }
  }

  /* --------------------------------------------------
   *  Helper GUI System
   * ------------------------------------------------*/
  function loadLilGui() {
    if (lilGuiLoaded) {
      return Promise.resolve();
    }
    if (lilGuiLoadPromise) {
      return lilGuiLoadPromise;
    }

    lilGuiLoadPromise = new Promise((resolve, reject) => {
      if (typeof lil !== "undefined") {
        lilGuiLoaded = true;
        resolve();
        return;
      }

      const script = document.createElement("script");
      script.src =
        "https://cdn.jsdelivr.net/npm/lil-gui@0.19.1/dist/lil-gui.umd.min.js";
      script.integrity =
        "sha384-2eNPNc7Cms+nVcpmQPotBpthLWCwjAGbkp0Y+3MUQqwPbmTpMFmbh2a230Gkns0x";
      script.crossOrigin = "anonymous";
      script.onload = () => {
        lilGuiLoaded = true;
        injectHelperStyles();
        resolve();
      };
      script.onerror = () => {
        reject(new Error("Failed to load lil-gui"));
      };
      document.head.appendChild(script);
    });

    return lilGuiLoadPromise;
  }

  function injectHelperStyles() {
    if (document.getElementById("spectragl-helper-styles")) return;

    const style = document.createElement("style");
    style.id = "spectragl-helper-styles";
    style.textContent = `
      @media screen and (max-width: 768px) {
        .lil-gui.root.spectragl-helper {
          width: 61vw;
        }
      }

      .lil-gui.root.spectragl-helper {
        --background-color: rgb(9 9 11 / 85%);
        --widget-color: rgb(39 39 42 / 50%);
        --hover-color: rgb(39 39 42 / 70%);
        --focus-color: rgb(39 39 42 / 90%);
        --number-color: #fafafa;
        --string-color: #fafafa;
        --font-size: 13px;
        --input-font-size: 13px;
        --font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif;
        --font-family-mono: monospace;
        --padding: 10px;
        --spacing: 10px;
        --widget-height: 28px;
        --title-height: 28px;
        --name-width: 45%;
        --slider-knob-width: 4px;
        --slider-input-width: 27%;
        --color-input-width: 27%;
        --slider-input-min-width: 45px;
        --color-input-min-width: 45px;
        --folder-indent: 8px;
        --widget-padding: 0 10px;
        --widget-border-radius: 4px;
        --checkbox-size: 16px;
        --scrollbar-width: 6px;
        border-radius: 12px !important;
        border: 0.5px solid #1e1e20 !important;
        backdrop-filter: blur(16px);
        box-shadow: 0 4px 16px rgb(0 0 0 / 20%) !important;
        position: fixed !important;
        top: 1rem !important;
        right: 1rem !important;
        left: auto !important;
        z-index: 999999999 !important;
      }

      .lil-gui.spectragl-helper .title {
        background: transparent;
        border-bottom: 0.5px solid #1e1e20;
        border-radius: 12px 12px 0 0 !important;
      }

      .lil-gui.spectragl-helper .title button {
        padding: 12px 16px !important;
      }

      .lil-gui.spectragl-helper.closed .title {
        border-radius: 12px !important;
        border-bottom: none;
      }

      .lil-gui.spectragl-helper .children {
        border-top: 0.5px solid #1e1e20;
      }

      @media (max-width: 768px) {
        .lil-gui.root.spectragl-helper {
          top: 1rem !important;
          right: 1rem !important;
        }
      }
    `;
    document.head.appendChild(style);
  }

  function createHelperGUI(instance, instanceIndex) {
    if (typeof lil === "undefined") return;

    const gui = new lil.GUI({
      title: `spectraGL Helper ${instanceIndex + 1}`,
      closeFolders: true,
    });
    gui.domElement.classList.add("spectragl-helper");
    gui.$title.style.cursor = "default";

    const topOffset = 1 + instanceIndex * 3;
    gui.domElement.style.top = `${topOffset}rem`;

    const opts = instance.options;

    const colorState = {
      color1: opts.colors[0] || "#ff6b6b",
      color2: opts.colors[1] || "#4ecdc4",
      color3: opts.colors[2] || "#45b7d1",
      color4: opts.colors[3] || "#f9ca24",
      color5: opts.colors[4] || "#6c5ce7",
      color6: opts.colors[5] || "#00b894",
      color7: opts.colors[6] || "#fd79a8",
      rimColor: opts.rimColor || "#ffffff",
      colorBlend: opts.colorBlend || "smooth",
      balance1: opts.colorBalance[0] || 1.0,
      balance2: opts.colorBalance[1] || 1.0,
      balance3: opts.colorBalance[2] || 1.0,
      balance4: opts.colorBalance[3] || 1.0,
      balance5: opts.colorBalance[4] || 1.0,
      balance6: opts.colorBalance[5] || 1.0,
      balance7: opts.colorBalance[6] || 1.0,
    };

    const updateColors = () => {
      instance.updateOptions({
        colors: [
          colorState.color1,
          colorState.color2,
          colorState.color3,
          colorState.color4,
          colorState.color5,
          colorState.color6,
          colorState.color7,
        ],
      });
    };

    const updateBalance = () => {
      instance.updateOptions({
        colorBalance: [
          colorState.balance1,
          colorState.balance2,
          colorState.balance3,
          colorState.balance4,
          colorState.balance5,
          colorState.balance6,
          colorState.balance7,
        ],
      });
    };

    const colorsFolder = gui.addFolder("Colors");
    colorsFolder
      .addColor(colorState, "color1")
      .name("Color 1")
      .onChange(updateColors);
    colorsFolder
      .addColor(colorState, "color2")
      .name("Color 2")
      .onChange(updateColors);
    colorsFolder
      .addColor(colorState, "color3")
      .name("Color 3")
      .onChange(updateColors);
    colorsFolder
      .addColor(colorState, "color4")
      .name("Color 4")
      .onChange(updateColors);
    colorsFolder
      .addColor(colorState, "color5")
      .name("Color 5")
      .onChange(updateColors);
    colorsFolder
      .addColor(colorState, "color6")
      .name("Color 6")
      .onChange(updateColors);
    colorsFolder
      .addColor(colorState, "color7")
      .name("Color 7")
      .onChange(updateColors);
    colorsFolder
      .addColor(colorState, "rimColor")
      .name("Rim Color")
      .onChange((value) => {
        instance.updateOptions({ rimColor: value });
      });
    colorsFolder
      .add(colorState, "colorBlend", ["smooth", "sharp", "stepped"])
      .name("Blend Mode")
      .onChange((value) => {
        instance.updateOptions({ colorBlend: value });
      });
    colorsFolder
      .add(colorState, "balance1", 0, 2, 0.1)
      .name("Balance 1")
      .onChange(updateBalance);
    colorsFolder
      .add(colorState, "balance2", 0, 2, 0.1)
      .name("Balance 2")
      .onChange(updateBalance);
    colorsFolder
      .add(colorState, "balance3", 0, 2, 0.1)
      .name("Balance 3")
      .onChange(updateBalance);
    colorsFolder
      .add(colorState, "balance4", 0, 2, 0.1)
      .name("Balance 4")
      .onChange(updateBalance);
    colorsFolder
      .add(colorState, "balance5", 0, 2, 0.1)
      .name("Balance 5")
      .onChange(updateBalance);
    colorsFolder
      .add(colorState, "balance6", 0, 2, 0.1)
      .name("Balance 6")
      .onChange(updateBalance);
    colorsFolder
      .add(colorState, "balance7", 0, 2, 0.1)
      .name("Balance 7")
      .onChange(updateBalance);

    const animFolder = gui.addFolder("Animation");
    const animState = {
      speed: opts.speed || 1.0,
      foldSpeed: opts.foldSpeed || 1.0,
      direction: opts.direction || "auto",
    };
    animFolder
      .add(animState, "speed", 0, 5, 0.1)
      .name("Speed")
      .onChange((value) => {
        instance.updateOptions({ speed: value });
      });
    animFolder
      .add(animState, "foldSpeed", 0, 5, 0.1)
      .name("Fold Speed")
      .onChange((value) => {
        instance.updateOptions({ foldSpeed: value });
      });
    animFolder
      .add(animState, "direction", ["auto", "up", "down", "left", "right"])
      .name("Direction")
      .onChange((value) => {
        instance.updateOptions({ direction: value });
      });

    const geoFolder = gui.addFolder("Geometry");
    const geoState = {
      foldIntensity: opts.foldIntensity || 0.5,
      foldScale: opts.foldScale || 1.0,
    };
    geoFolder
      .add(geoState, "foldIntensity", 0, 2, 0.05)
      .name("Fold Intensity")
      .onChange((value) => {
        instance.updateOptions({ foldIntensity: value });
      });
    geoFolder
      .add(geoState, "foldScale", 0, 5, 0.1)
      .name("Fold Scale")
      .onChange((value) => {
        instance.updateOptions({ foldScale: value });
      });

    const lightFolder = gui.addFolder("Lighting");
    const lightState = {
      rimLight: opts.rimLight !== undefined ? opts.rimLight : true,
      rimIntensity: opts.rimIntensity || 0.8,
      rimFalloff: opts.rimFalloff || 2.0,
      grain: opts.grain || 0.05,
    };
    lightFolder
      .add(lightState, "rimLight")
      .name("Rim Light")
      .onChange((value) => {
        instance.updateOptions({ rimLight: value });
      });
    lightFolder
      .add(lightState, "rimIntensity", 0, 1, 0.05)
      .name("Rim Intensity")
      .onChange((value) => {
        instance.updateOptions({ rimIntensity: value });
      });
    lightFolder
      .add(lightState, "rimFalloff", 0.5, 5.0, 0.1)
      .name("Rim Falloff")
      .onChange((value) => {
        instance.updateOptions({ rimFalloff: value });
      });
    lightFolder
      .add(lightState, "grain", 0, 0.3, 0.01)
      .name("Grain")
      .onChange((value) => {
        instance.updateOptions({ grain: value });
      });

    const interactFolder = gui.addFolder("Interaction");
    const interactState = {
      reactive: opts.reactive !== undefined ? opts.reactive : true,
      reactiveStrength: opts.reactiveStrength || 0.3,
      displacementStrength: opts.displacementStrength || 0.3,
      mouseRadius: opts.mouseRadius || 0.15,
      scrollReactive:
        opts.scrollReactive !== undefined ? opts.scrollReactive : false,
      scrollStrength: opts.scrollStrength || 0.2,
    };
    interactFolder
      .add(interactState, "reactive")
      .name("Reactive")
      .onChange((value) => {
        instance.updateOptions({ reactive: value });
      });
    interactFolder
      .add(interactState, "reactiveStrength", 0, 2, 0.05)
      .name("Reactive Strength")
      .onChange((value) => {
        instance.updateOptions({ reactiveStrength: value });
      });
    interactFolder
      .add(interactState, "displacementStrength", 0, 2, 0.05)
      .name("Displacement Strength")
      .onChange((value) => {
        instance.updateOptions({ displacementStrength: value });
      });
    interactFolder
      .add(interactState, "mouseRadius", 0, 1, 0.05)
      .name("Mouse Radius")
      .onChange((value) => {
        instance.updateOptions({ mouseRadius: value });
      });
    interactFolder
      .add(interactState, "scrollReactive")
      .name("Scroll Reactive")
      .onChange((value) => {
        instance.updateOptions({ scrollReactive: value });
      });
    interactFolder
      .add(interactState, "scrollStrength", 0, 2, 0.05)
      .name("Scroll Strength")
      .onChange((value) => {
        instance.updateOptions({ scrollStrength: value });
      });

    const borderFolder = gui.addFolder("Border");
    const borderState = {
      enabled: opts.border.enabled || false,
      width: opts.border.width || 2,
      radius: opts.border.radius || 0,
    };
    borderFolder
      .add(borderState, "enabled")
      .name("Enabled")
      .onChange((value) => {
        instance.updateOptions({ border: { ...opts.border, enabled: value } });
      });
    borderFolder
      .add(borderState, "width", 1, 20, 1)
      .name("Width")
      .onChange((value) => {
        instance.updateOptions({ border: { ...opts.border, width: value } });
      });
    borderFolder
      .add(borderState, "radius", 0, 50, 1)
      .name("Radius")
      .onChange((value) => {
        instance.updateOptions({ border: { ...opts.border, radius: value } });
      });

    const hardReinitState = {
      mode: opts.mode || "aurora",
      meshDetail: opts.meshDetail || 32,
      target: opts.target || ".spectraGL",
    };

    gui
      .add(hardReinitState, "mode", [
        "aurora",
        "shimmer",
        "pixel",
        "komorebi",
        "nebula",
        "floral",
        "oil",
        "diva",
      ])
      .name("Mode")
      .onFinishChange((value) => {
        if (confirm("Changing mode requires reinitialization. Continue?")) {
          instance.updateOptions({ mode: value });
        } else {
          hardReinitState.mode = opts.mode;
          gui.controllersRecursive().forEach((c) => c.updateDisplay());
        }
      });

    gui
      .add(hardReinitState, "meshDetail", 16, 128, 1)
      .name("Mesh Detail")
      .onFinishChange((value) => {
        if (
          confirm("Changing mesh detail requires reinitialization. Continue?")
        ) {
          instance.updateOptions({ meshDetail: value });
        } else {
          hardReinitState.meshDetail = opts.meshDetail;
          gui.controllersRecursive().forEach((c) => c.updateDisplay());
        }
      });

    const qualityState = {
      qualityPreset: opts.qualityPreset || "balanced",
      pixelRatio: opts.pixelRatio || "auto",
      maxFPS: opts.maxFPS || 60,
    };

    gui
      .add(qualityState, "qualityPreset", ["low", "balanced", "high"])
      .name("Quality Preset")
      .onChange((value) => {
        instance.updateOptions({ qualityPreset: value });
      });

    gui
      .add(qualityState, "pixelRatio", ["auto", "1", "2"])
      .name("Pixel Ratio")
      .onChange((value) => {
        instance.updateOptions({
          pixelRatio: value === "auto" ? "auto" : parseFloat(value),
        });
      });

    gui
      .add(qualityState, "maxFPS", 15, 120, 1)
      .name("Max FPS")
      .onChange((value) => {
        instance.updateOptions({ maxFPS: value });
      });

    const copyButton = {
      copySettings: () => {
        const code = generateInitCode(instance.options);
        const controller = gui.controllers.find(
          (c) => c.property === "copySettings"
        );

        if (!controller) return;

        const originalName = controller._name;
        controller.disable();
        controller.name("✓ Copied");

        const copySuccess = () => {
          setTimeout(() => {
            controller.name(originalName);
            controller.enable();
          }, 1500);
        };

        if (navigator.clipboard && navigator.clipboard.writeText) {
          navigator.clipboard
            .writeText(code)
            .then(() => {
              copySuccess();
            })
            .catch((err) => {
              console.error("Clipboard error:", err);
              fallbackCopy(code);
            });
        } else {
          fallbackCopy(code);
        }

        function fallbackCopy(text) {
          const textarea = document.createElement("textarea");
          textarea.value = text;
          textarea.style.position = "fixed";
          textarea.style.opacity = "0";
          document.body.appendChild(textarea);
          textarea.select();
          try {
            document.execCommand("copy");
            copySuccess();
          } catch (err) {
            console.error("Copy failed:", err);
            controller.name(originalName);
            controller.enable();
            prompt("Copy this code manually:", text);
          }
          document.body.removeChild(textarea);
        }
      },
    };
    gui.add(copyButton, "copySettings").name("Copy settings");

    colorsFolder.close();
    animFolder.close();
    geoFolder.close();
    lightFolder.close();
    interactFolder.close();
    borderFolder.close();

    helperGUIs.push({ gui, instance });
    return gui;
  }

  function generateInitCode(options) {
    const lines = ["spectraGL({"];

    lines.push(`  target: "${options.target}",`);
    lines.push(`  mode: "${options.mode}",`);

    const colors = options.colors.map((c) => `"${c}"`).join(", ");
    lines.push(`  colors: [${colors}],`);

    lines.push(`  colorBlend: "${options.colorBlend}",`);

    const balance = options.colorBalance.join(", ");
    lines.push(`  colorBalance: [${balance}],`);

    lines.push(`  meshDetail: ${options.meshDetail},`);
    lines.push(`  foldIntensity: ${options.foldIntensity},`);
    lines.push(`  foldScale: ${options.foldScale},`);
    lines.push(`  foldSpeed: ${options.foldSpeed},`);
    lines.push(`  rimLight: ${options.rimLight},`);
    lines.push(`  rimIntensity: ${options.rimIntensity},`);
    lines.push(`  rimColor: "${options.rimColor}",`);
    lines.push(`  rimFalloff: ${options.rimFalloff},`);
    lines.push(`  speed: ${options.speed},`);
    lines.push(`  direction: "${options.direction}",`);
    lines.push(`  grain: ${options.grain},`);
    lines.push(`  reactive: ${options.reactive},`);
    lines.push(`  reactiveStrength: ${options.reactiveStrength},`);
    lines.push(`  displacementStrength: ${options.displacementStrength},`);
    lines.push(`  mouseRadius: ${options.mouseRadius},`);
    lines.push(`  scrollReactive: ${options.scrollReactive},`);
    lines.push(`  scrollStrength: ${options.scrollStrength},`);
    lines.push(`  pixelRatio: "${options.pixelRatio}",`);
    lines.push(`  maxFPS: ${options.maxFPS},`);
    lines.push(`  qualityPreset: "${options.qualityPreset}",`);
    lines.push(`  border: {`);
    lines.push(`    enabled: ${options.border.enabled},`);
    lines.push(`    width: ${options.border.width},`);
    lines.push(`    radiusFromElement: ${options.border.radiusFromElement},`);
    lines.push(`    radius: ${options.border.radius},`);
    lines.push(`    position: "${options.border.position}",`);
    lines.push(`  },`);
    lines.push(`  helper: false,`);
    lines.push(`});`);

    return lines.join("\n");
  }

  function destroyHelperGUI(instance) {
    const index = helperGUIs.findIndex((h) => h.instance === instance);
    if (index !== -1) {
      const { gui } = helperGUIs[index];
      gui.destroy();
      helperGUIs.splice(index, 1);

      helperGUIs.forEach((h, i) => {
        const topOffset = 1 + i * 3;
        h.gui.domElement.style.top = `${topOffset}rem`;
      });
    }
  }

  /* --------------------------------------------------
   *  Public API
   * ------------------------------------------------*/
  window.spectraGL = function (userOptions = {}) {
    const defaults = {
      target: ".spectraGL",
      mode: "aurora",
      colors: [
        "#ff6b6b",
        "#4ecdc4",
        "#45b7d1",
        "#f9ca24",
        "#6c5ce7",
        "#00b894",
        "#fd79a8",
      ],
      colorBlend: "smooth",
      colorBalance: [1, 1, 1, 1, 1, 1, 1],
      meshDetail: 32,
      foldIntensity: 0.5,
      foldScale: 1.0,
      foldSpeed: 1.0,
      rimLight: true,
      rimIntensity: 0.8,
      rimColor: "#ffffff",
      rimFalloff: 2.0,
      speed: 1.0,
      direction: "auto",
      grain: 0.05,
      reactive: true,
      reactiveStrength: 0.3,
      displacementStrength: 0.3,
      mouseRadius: 0.15,
      scrollReactive: false,
      scrollStrength: 0.2,
      pixelRatio: "auto",
      maxFPS: 60,
      qualityPreset: "balanced",
      border: {
        enabled: false,
        width: 2,
        radiusFromElement: true,
        radius: null,
        position: "outside",
      },
      on: {},
      helper: false,
    };

    const options = {
      ...defaults,
      ...userOptions,
      border: { ...defaults.border, ...(userOptions.border || {}) },
      on: { ...defaults.on, ...(userOptions.on || {}) },
    };

    if (!userOptions.qualityPreset && isLowEndDevice()) {
      options.qualityPreset = "low";
    }

    if (options.qualityPreset && !userOptions.meshDetail) {
      const qualitySettings = {
        low: { meshDetail: 16, maxFPS: 30 },
        balanced: { meshDetail: 32, maxFPS: 60 },
        high: { meshDetail: 64, maxFPS: 60 },
      };
      const preset = qualitySettings[options.qualityPreset];
      if (preset) {
        options.meshDetail = preset.meshDetail;
        if (!userOptions.maxFPS) options.maxFPS = preset.maxFPS;
      }
    }

    // Safari: reduce FPS for heavy modes
    let effectiveMaxFPS = options.maxFPS || 60;
    if (isSafari() && !userOptions.maxFPS) {
      const heavyModes = ["nebula", "oil", "floral"];
      const hasHeavyMode = Array.from(
        document.querySelectorAll(options.target)
      ).some((el) => {
        const mode = options.mode || "aurora";
        return heavyModes.includes(mode);
      });
      if (hasHeavyMode) {
        effectiveMaxFPS = 30;
      }
    }

    targetFrameTime = 1000 / effectiveMaxFPS;

    const elements = document.querySelectorAll(options.target);
    if (elements.length === 0) {
      console.error(`spectraGL: No elements found for "${options.target}"`);
      return null;
    }

    const instance = new SpectraGLInstance(options, Array.from(elements));
    instance.init();

    return {
      pause: () => instance.pause(),
      play: () => instance.play(),
      destroy: () => instance.destroy(),
      updateOptions: (opts) => instance.updateOptions(opts),
      get isPlaying() {
        return !instance.paused;
      },
      get options() {
        return instance.options;
      },
      get supported() {
        return instance.supported;
      },
      get fallbackApplied() {
        return instance.fallbackApplied;
      },
    };
  };

  window.spectraGL.isSupported = () => WebGLSupport.getSupported();
  window.spectraGL.version = "1.0.0";
})();
