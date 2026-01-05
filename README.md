# spectraGL – WebGL Interactive Effects Library

<p align="center">
  <a href="https://spectragl.naughtyduk.com">
    <img src="/assets/spectraGL-promo-IMG.gif" alt="spectraGL" width="100%"/>
  </a>
</p>

**v1.0.0**

> [!NOTE]
> `spectraGL` uses a dual licence model. It is **free for personal use**. `spectraGL` requires a licence for commercial use, see the [licensing section](#licence) for more details.

`spectraGL` transforms any DOM element into beautiful, interactive light and colour effects with mouse reactivity, rendered in high-performance WebGL. It works on divs, buttons, text etc.

<a href="https://spectragl.naughtyduk.com" target="_blank" rel="noopener noreferrer"><img src="./assets/try-btn.svg" alt="Try It Out Button"></a>

**DEMOS**

<a href="https://spectragl.naughtyduk.com/demos/demo-elements.html" target="_blank" rel="noopener noreferrer"><strong>ELEMENTS</strong></a> | <a href="https://spectragl.naughtyduk.com/demos/aurora.html" target="_blank" rel="noopener noreferrer"><strong>AURORA</strong></a> | <a href="https://spectragl.naughtyduk.com/demos/shimmer.html" target="_blank" rel="noopener noreferrer"><strong>SHIMMER</strong></a> | <a href="https://spectragl.naughtyduk.com/demos/pixel.html" target="_blank" rel="noopener noreferrer"><strong>PIXEL</strong></a> | <a href="https://spectragl.naughtyduk.com/demos/komorebi.html" target="_blank" rel="noopener noreferrer"><strong>KOMOREBI</strong></a> | <a href="https://spectragl.naughtyduk.com/demos/nebula.html" target="_blank" rel="noopener noreferrer"><strong>NEBULA</strong></a> | <a href="https://spectragl.naughtyduk.com/demos/floral.html" target="_blank" rel="noopener noreferrer"><strong>FLORAL</strong></a> | <a href="https://spectragl.naughtyduk.com/demos/oil.html" target="_blank" rel="noopener noreferrer"><strong>OIL</strong></a> | <a href="https://spectragl.naughtyduk.com/demos/diva.html" target="_blank" rel="noopener noreferrer"><strong>DIVA</strong></a>

## Overview

`spectraGL` brings interactive light and colour effects to the web with a high-performance WebGL renderer. It converts any DOM element into stunning visual effects using procedurally generated shaders. The library features eight distinct visual modes—from flowing aurora lights to cosmic nebulae, abstract florals to iridescent shimmer—each with mouse reactivity, scroll interactions, and extensive customisation options.

### Key Features

| Feature                       | Supported | Feature                      | Supported |
| :---------------------------- | :-------: | :--------------------------- | :-------: |
| 8 Visual Effect Modes         |    ✅     | Mouse Displacement Effects   |    ✅     |
| Text Masking Support          |    ✅     | Scroll Reactivity            |    ✅     |
| Click/Tap Pulse Effects       |    ✅     | Velocity-Based Displacement  |    ✅     |
| Procedural Shader Generation  |    ✅     | 7-Colour Spectrum Support    |    ✅     |
| Directional Flow Control      |    ✅     | Rim Lighting Effects         |    ✅     |
| 3D Geometry Displacement      |    ✅     | Multiple Instances           |    ✅     |
| Border Mode                   |    ✅     | Auto-Resize Handling         |    ✅     |
| Smooth/Sharp/Stepped Blending |    ✅     | Performance Optimisation     |    ✅     |
| Quality Presets               |    ✅     | `on.init` Callback           |    ✅     |
| WebGL1 & WebGL2 Support       |    ✅     | Smart DOM Positioning        |    ✅     |
| Grain & Texture Control       |    ✅     | Low-End Device Detection     |    ✅     |
| Live GUI Helper               |    ✅     | Real-Time Parameter Tweaking |    ✅     |

---

## Prerequisites

Add the following scripts before you initialise `spectraGL()` (normally at the end of the `<body>`):

```html
<!-- Three.js – WebGL 3D library (required) -->
<script src="https://cdnjs.cloudflare.com/ajax/libs/three.js/r128/three.min.js"></script>

<!-- spectraGL.min.js – the library itself -->
<script src="/scripts/spectraGL.min.js"></script>
```

> `Three.js` provides the WebGL rendering engine that powers `spectraGL`. The library will not work without Three.js.

---

## Quick Start

Set up your HTML structure first. Add the `spectraGL` class to any element you want to apply shader effects to.

```html
<!-- Example HTML structure -->
<body>
  <div class="hero-section">
    <!-- Target element (will have shader effect applied) -->
    <div class="spectraGL"></div>
  </div>

  <!-- AND/OR use with text (auto-detected and masked) -->
  <h1 class="spectraGL">Hello World</h1>

  <!-- AND/OR with buttons (border mode) -->
  <button class="spectraGL-border">Click Me</button>

  <!-- AND/OR any other element -->
</body>
```

> The element will have the shader effect applied directly to it. Text elements are automatically detected and masked. Border Mode is available for buttons, cards etc.

Next, initialise the library with your desired configuration.

```html
<script>
  document.addEventListener("DOMContentLoaded", () => {
    const effect = spectraGL({
      target: ".spectraGL", // CSS selector for the element(s) to apply effects
      mode: "aurora", // Visual mode: aurora, shimmer, pixel, komorebi, nebula, floral, oil, diva

      // Colours (7-colour spectrum for rainbow modes)
      colors: [
        "#ff6b6b",
        "#4ecdc4",
        "#45b7d1",
        "#f9ca24",
        "#6c5ce7",
        "#00b894",
        "#fd79a8",
      ],
      colorBlend: "smooth", // Blend mode: smooth, sharp, stepped
      colorBalance: [1, 1, 1, 1, 1, 1, 1], // Individual colour intensities

      // Geometry & Animation
      meshDetail: 32, // Mesh subdivision (16=low, 32=balanced, 64=high)
      foldIntensity: 0.5, // Effect intensity/complexity
      foldScale: 1.0, // Pattern scale
      foldSpeed: 1.0, // Animation speed multiplier
      speed: 1.0, // Global speed control
      direction: "auto", // Flow direction: auto, up, down, left, right

      // Lighting
      rimLight: true, // Enable rim/edge lighting
      rimIntensity: 0.8, // Rim light intensity
      rimColor: "#ffffff", // Rim light colour
      rimFalloff: 2.0, // Rim light falloff

      // Texture
      grain: 0.05, // Film grain amount

      // Reactivity
      reactive: true, // Enable mouse interaction
      reactiveStrength: 0.3, // Mouse influence strength
      displacementStrength: 0.3, // Velocity-based displacement
      mouseRadius: 0.15, // Mouse influence radius
      scrollReactive: false, // Enable scroll interaction
      scrollStrength: 0.2, // Scroll influence strength

      // Performance
      qualityPreset: "balanced", // Quality: low, balanced, high
      maxFPS: 60, // Frame rate limit: 30, 60, 120

      // Border Mode (for buttons, cards etc.)
      border: {
        enabled: false, // Enable border mode
        width: 2, // Border width in pixels
        radius: null, // Border radius (null = inherit from element)
      },

      // Development Helper
      helper: false, // Enable to design visually and copy code, disable for production

      on: {
        init(instance) {
          // The `init` callback fires once spectraGL has initialised
          // and rendered the first frame
          console.log("spectraGL ready!", instance);
        },
      },
    });
  });
</script>
```

**Visual Modes**

spectraGL includes eight distinct shader-based visual modes:

| Mode       | Description                                                                                    |
| ---------- | ---------------------------------------------------------------------------------------------- |
| `aurora`   | Flowing caustic light patterns with rainbow colour dispersion, premium gradient dispersion.    |
| `shimmer`  | Holographic shimmer effect with flowing colour dispersion and visible wave edges.              |
| `pixel`    | Dithered pixel art effect with Bayer matrix dithering and organic FBM noise.                   |
| `komorebi` | Japanese for light shining through trees, dark flowing patterns with organic FBM smokey waves. |
| `nebula`   | Realistic cosmic nebula with layered gas clouds and parallax scrolling.                        |
| `floral`   | Abstract flower petal patterns with procedurally generated blooms and organic textures.        |
| `oil`      | Iridescent oil slick effect with realistic lighting and chromatic reflections.                 |
| `diva`     | Chaotic data patterns with flowing gradients and iridescent colour transitions.                |

---

## Parameters

| Option                 | Type     | Default                                                                         | Description                                                                               |
| ---------------------- | -------- | ------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------- |
| `target`               | string   | `'.spectraGL'`                                                                  | **Required.** CSS selector for the element(s) to apply effects.                           |
| `mode`                 | string   | `'aurora'`                                                                      | Visual mode: `aurora`, `shimmer`, `pixel`, `komorebi`, `nebula`, `floral`, `oil`, `diva`. |
| `colors`               | array    | `['#ff6b6b', '#4ecdc4', '#45b7d1', '#f9ca24', '#6c5ce7', '#00b894', '#fd79a8']` | Array of hex colour codes (up to 7 for rainbow spectrum modes).                           |
| `colorBlend`           | string   | `'smooth'`                                                                      | Colour blending mode: `smooth`, `sharp`, `stepped`.                                       |
| `colorBalance`         | array    | `[1,1,1...]`                                                                    | Individual colour intensity multipliers (0–2 for each colour).                            |
| `meshDetail`           | number   | `32`                                                                            | Mesh subdivision quality (16=low, 32=balanced, 64=high).                                  |
| `foldIntensity`        | number   | `0.5`                                                                           | Effect intensity/complexity (0–2).                                                        |
| `foldScale`            | number   | `1.0`                                                                           | Pattern scale multiplier (0.1–5).                                                         |
| `foldSpeed`            | number   | `1.0`                                                                           | Animation speed multiplier (0.1–5).                                                       |
| `speed`                | number   | `1.0`                                                                           | Global speed control (0.1–5).                                                             |
| `direction`            | string   | `'auto'`                                                                        | Flow direction: `auto`, `up`, `down`, `left`, `right`.                                    |
| `rimLight`             | boolean  | `true`                                                                          | Enable rim/edge lighting effects.                                                         |
| `rimIntensity`         | number   | `0.8`                                                                           | Rim light intensity (0–2).                                                                |
| `rimColor`             | string   | `'#ffffff'`                                                                     | Rim light colour (hex code).                                                              |
| `rimFalloff`           | number   | `2.0`                                                                           | Rim light falloff/threshold (0–5).                                                        |
| `grain`                | number   | `0.05`                                                                          | Film grain amount (0–0.2).                                                                |
| `reactive`             | boolean  | `true`                                                                          | Enable mouse interaction.                                                                 |
| `reactiveStrength`     | number   | `0.3`                                                                           | Mouse influence strength (0–2).                                                           |
| `displacementStrength` | number   | `0.3`                                                                           | Velocity-based displacement strength (0–2).                                               |
| `mouseRadius`          | number   | `0.15`                                                                          | Mouse influence radius (0.05–0.5).                                                        |
| `scrollReactive`       | boolean  | `false`                                                                         | Enable scroll wheel interaction.                                                          |
| `scrollStrength`       | number   | `0.2`                                                                           | Scroll influence strength (0–1).                                                          |
| `qualityPreset`        | string   | `'balanced'`                                                                    | Quality preset: `low`, `balanced`, `high`.                                                |
| `maxFPS`               | number   | `60`                                                                            | Frame rate limit: `30`, `60`, `120`.                                                      |
| `border.enabled`       | boolean  | `false`                                                                         | Enable border mode (for buttons).                                                         |
| `border.width`         | number   | `2`                                                                             | Border width in pixels.                                                                   |
| `border.radius`        | number   | `null`                                                                          | Border radius (null = inherit from element).                                              |
| `helper`               | boolean  | `false`                                                                         | Enable live GUI for real-time parameter tweaking (development tool).                      |
| `on.init`              | function | `null`                                                                          | Callback that runs once the effect is ready. Receives the instance as an argument.        |

> The `target` parameter is required; all others are optional.

---

## Presets

Below are some ready-made configurations for different effects:

| Name            | Settings                                                                                                 | Purpose                              |
| --------------- | -------------------------------------------------------------------------------------------------------- | ------------------------------------ |
| **Ethereal**    | `{ mode: 'aurora', foldIntensity: 0.3, speed: 0.5, colorBlend: 'smooth' }`                               | Gentle, flowing light effect.        |
| **Cosmic**      | `{ mode: 'nebula', foldIntensity: 1.2, foldScale: 2.0, rimLight: true }`                                 | Deep space nebula with rich colours. |
| **Botanical**   | `{ mode: 'floral', foldScale: 1.5, foldSpeed: 0.8, colorBlend: 'sharp' }`                                | Crisp floral patterns.               |
| **Iridescent**  | `{ mode: 'oil', foldIntensity: 1.5, rimIntensity: 1.2, reactiveStrength: 0.5 }`                          | Strong oil slick reflections.        |
| **Retro**       | `{ mode: 'pixel', foldIntensity: 1.0, grain: 0.1, colorBlend: 'stepped' }`                               | Pixel-art dithered aesthetic.        |
| **Holographic** | `{ mode: 'shimmer', foldIntensity: 0.8, speed: 1.5, colors: ['#ff00ff', '#00ffff', '#ffff00'] }`         | Fast-moving holographic shimmer.     |
| **Dark Flow**   | `{ mode: 'komorebi', foldScale: 1.2, speed: 0.7, colors: ['#000000', '#1a1a2e', '#16213e', '#0f3460'] }` | Dark, mysterious flowing patterns.   |

---

## FAQ

| Question                                      | Answer                                                                                                                                                                                                                                                                                                                                          |
| :-------------------------------------------- | :---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Does the library handle responsive design?    | Yes, `spectraGL` automatically handles window resize events and rebuilds effects as needed. Resize handling is debounced to 250ms for performance.                                                                                                                                                                                              |
| What happens to the original element?         | The original element remains in the DOM but is hidden (`visibility: hidden` for standard elements, `color: transparent` for text elements). A WebGL canvas is inserted before the element in the DOM and positioned to match the element's dimensions. The original element maintains page layout whilst the canvas provides the visual effect. |
| How does text masking work?                   | Text elements (h1-h6, p, span, etc.) are automatically detected. The library generates a canvas-based text mask that clips the shader effect to the text shape, preserving the original typography whilst applying the visual effect.                                                                                                           |
| How do I optimise performance?                | Use the `qualityPreset` option (`low`, `balanced`, `high`). Lower `meshDetail` reduces geometry complexity. Set `maxFPS` to 30 for slower devices. The library automatically detects low-end devices and applies optimisations, including viewport culling for off-screen effects.                                                              |
| Can I update properties after initialisation? | Yes, use the `updateOptions()` method: `effect.updateOptions({ foldIntensity: 1.5, colors: ['#ff0000', '#00ff00'] })`. Most properties update in real-time. Only fundamental changes (like `target` or `mode`) will cause the effect to be rebuilt, which the library handles automatically.                                                    |
| Does the effect work on mobile devices?       | Yes, `spectraGL` works on most modern mobile devices that support WebGL. Touch interactions work the same as mouse events. The library automatically applies performance optimisations for mobile devices.                                                                                                                                      |
| What types of elements can be converted?      | Any DOM element can have a shader effect applied. Text elements are automatically masked to preserve typography. Buttons can use border mode. The effect replaces the element's visual appearance whilst maintaining its layout position.                                                                                                       |
| How does scroll reactivity work?              | When `scrollReactive: true`, scrolling over an element (mouse wheel or touch drag) creates a velocity-based offset that warps the shader pattern. The effect smoothly decays back to the original state. Scroll strength is controlled by `scrollStrength`.                                                                                     |
| What is border mode used for?                 | Border mode (`border.enabled: true`) is designed for buttons and interactive elements. It creates an animated border around the element whilst keeping the element's content visible. The border width and radius are customisable.                                                                                                             |
| Can I use multiple instances on one page?     | Yes, `spectraGL` supports multiple instances with different configurations. All instances share a single WebGL renderer for optimal performance. Each instance can have its own mode, colours, and settings.                                                                                                                                    |

---

## Browser Support

The `spectraGL` library is compatible with all modern WebGL-enabled browsers on desktop, tablet, and mobile devices.

| Browser        | Supported |
| :------------- | :-------: |
| Google Chrome  |    ✅     |
| Safari         |    ✅     |
| Firefox        |    ✅     |
| Microsoft Edge |    ✅     |
| Mobile Safari  |    ✅     |
| Mobile Chrome  |    ✅     |

> **Note**: Requires WebGL support. The library will fail to initialise if WebGL or Three.js are not available.

---

## Methods

After initialisation, you can control the effect using the methods on the returned instance:

```javascript
const effect = spectraGL({ target: ".spectraGL" });

// Pause the animation (stops rendering)
effect.pause();

// Resume the animation
effect.play();

// Update effect properties on the fly
effect.updateOptions({
  mode: "nebula",
  foldIntensity: 1.5,
  colors: ["#ff0000", "#00ff00", "#0000ff"],
});

// Access the current options object
console.log(effect.options);

// Check if the effect is currently playing
console.log(effect.isPlaying); // true/false

// Check WebGL support
console.log(effect.supported); // 'webgl2', 'webgl', or false

// Check if fallback was applied (CSS gradient)
console.log(effect.fallbackApplied); // true/false

// Clean up the effect and restore the original element
effect.destroy();
```

---

## Licence

`spectraGL` is released under a dual-licence model to support both personal and commercial use. For full details, please see the [LICENCE](./LICENCE.md) file.

### Personal Use

For personal websites, portfolios, academic projects, and other non-commercial applications, `spectraGL` is free to use. In short, if you are not making money from your project, you can use `spectraGL` for free.

### Commercial Use

A paid commercial licence is required for any project that is commercial in nature. This includes websites for businesses, projects that generate revenue, or use in any proprietary software.

### Licensing Options

**Single Licence:**<br>
`For one commercial website or project.`<br><br>
<a href="https://pay.naughtyduk.com/b/9B68wPaEGcovbNKbZP9sk0d" target="_blank" rel="noopener noreferrer"><img src="./assets/licence-btn.svg" alt="Get Licence Button"></a>

**Extended Licence:**<br>
`For up to five commercial projects.`<br><br>
<a href="https://pay.naughtyduk.com/b/28E3cv0029cj0527Jz9sk0e" target="_blank" rel="noopener noreferrer"><img src="./assets/licence-btn.svg" alt="Get Licence Button"></a>
