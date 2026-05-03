/**
 * Debug Panel Component
 * 
 * Provides sliders to adjust 3D carousel parameters in real-time.
 */

import { h } from "snabbdom/build/h.js";
import type { VNode } from "snabbdom/build/vnode.js";
import type { Carousel3DParams } from "../../types/state.js";

export interface DebugPanelCallbacks {
  readonly onToggle: () => void;
  readonly onParamChange: (param: keyof Carousel3DParams, value: number | boolean) => void;
}

interface SliderConfig {
  readonly param: keyof Carousel3DParams;
  readonly label: string;
  readonly min: number;
  readonly max: number;
  readonly step: number;
  readonly unit?: string;
  readonly defaultValue: number;
}

const sliderConfigs: ReadonlyArray<SliderConfig> = [
  { param: "radius", label: "Radius", min: 400, max: 3000, step: 50, unit: "px", defaultValue: 2300 },
  { param: "perspective", label: "Perspective", min: 200, max: 3000, step: 50, unit: "px", defaultValue: 1000 },
  { param: "anglePerCard", label: "Angle Per Card", min: 5, max: 45, step: 1, unit: "°", defaultValue: 15 },
  { param: "visibleAngleRange", label: "Visible Angle Range", min: 30, max: 180, step: 5, unit: "°", defaultValue: 60 },
  { param: "fadeTransitionAngle", label: "Fade Transition", min: 0, max: 30, step: 1, unit: "°", defaultValue: 10 },
  { param: "rotateX", label: "Rotate X", min: -180, max: 180, step: 1, unit: "°", defaultValue: 0 },
  { param: "rotateY", label: "Rotate Y", min: -180, max: 180, step: 1, unit: "°", defaultValue: 61 },
  { param: "rotateZ", label: "Rotate Z", min: -180, max: 180, step: 1, unit: "°", defaultValue: -90 },
  { param: "perspectiveOriginY", label: "Perspective Origin Y", min: 0, max: 200, step: 5, unit: "%", defaultValue: 110 },
  { param: "originX", label: "Origin X", min: -2000, max: 2000, step: 10, unit: "px", defaultValue: 0 },
  { param: "originY", label: "Origin Y", min: -2000, max: 2000, step: 10, unit: "px", defaultValue: 360 },
  { param: "originZ", label: "Origin Z", min: -2000, max: 2000, step: 10, unit: "px", defaultValue: 220 },
  { param: "cardScale", label: "Card Scale", min: 0.3, max: 2.0, step: 0.05, unit: "x", defaultValue: 1.7 },
  { param: "rotationSpeed", label: "Rotation Speed", min: 0.1, max: 2.0, step: 0.05, defaultValue: 0.3 },
];

function renderSlider(
  config: SliderConfig,
  value: number,
  callbacks: DebugPanelCallbacks
): VNode {
  const isDefault = Math.abs(value - config.defaultValue) < 0.001;
  
  return h("div.debug-slider", {
    style: {
      display: "flex",
      flexDirection: "column",
      gap: "4px",
      marginBottom: "12px",
    },
  }, [
    h("div", {
      style: {
        display: "flex",
        justifyContent: "space-between",
        fontSize: "0.75rem",
        color: "var(--text-secondary)",
        fontFamily: "var(--font-mono)",
      },
    }, [
      h("label", {
        style: {
          cursor: "pointer",
        },
        attrs: {
          title: "Double-click to reset to default",
        },
      }, config.label),
      h("span", {
        style: {
          color: isDefault ? "var(--text-secondary)" : "var(--accent-blue)",
          fontWeight: "600",
        },
      }, `${value.toFixed(config.step < 1 ? 2 : 0)}${config.unit || ""}`),
    ]),
    h("input", {
      props: {
        type: "range",
        min: config.min,
        max: config.max,
        step: config.step,
        value: value,
      },
      style: {
        width: "100%",
        accentColor: "var(--accent-blue)",
        cursor: "pointer",
      },
      attrs: {
        title: "Double-click to reset to default",
      },
      on: {
        input: (e: Event) => {
          const target = e.target as HTMLInputElement;
          callbacks.onParamChange(config.param, parseFloat(target.value));
        },
        dblclick: () => {
          callbacks.onParamChange(config.param, config.defaultValue);
        },
      },
    }),
  ]);
}

function renderToggle(
  label: string,
  param: keyof Carousel3DParams,
  value: boolean,
  callbacks: DebugPanelCallbacks
): VNode {
  return h("div.debug-toggle", {
    style: {
      display: "flex",
      justifyContent: "space-between",
      alignItems: "center",
      marginBottom: "12px",
    },
  }, [
    h("label", {
      style: {
        fontSize: "0.75rem",
        color: "var(--text-secondary)",
        fontFamily: "var(--font-mono)",
      },
    }, label),
    h("input", {
      props: {
        type: "checkbox",
        checked: value,
      },
      style: {
        accentColor: "var(--accent-blue)",
        cursor: "pointer",
      },
      on: {
        change: (e: Event) => {
          const target = e.target as HTMLInputElement;
          callbacks.onParamChange(param, target.checked);
        },
      },
    }),
  ]);
}

function printParamsToConsole(params: Carousel3DParams): void {
  const output = {
    radius: params.radius,
    perspective: params.perspective,
    rotationSpeed: params.rotationSpeed,
    anglePerCard: params.anglePerCard,
    visibleAngleRange: params.visibleAngleRange,
    fadeTransitionAngle: params.fadeTransitionAngle,
    rotateX: params.rotateX,
    rotateY: params.rotateY,
    rotateZ: params.rotateZ,
    perspectiveOriginY: params.perspectiveOriginY,
    originX: params.originX,
    originY: params.originY,
    originZ: params.originZ,
    cardScale: params.cardScale,
    showDebugDots: params.showDebugDots,
  };
  
  console.log("=== Current Carousel 3D Parameters ===");
  console.log(JSON.stringify(output, null, 2));
  console.log("\n📋 Copy the object above to update default values in main.ts");
  console.log("\nFor main.ts, use:");
  console.log(`
  debug: {
    enabled: false,
    params: ${JSON.stringify(output, null, 6).replace(/^/gm, '    ')},
  },
  `.trim());
}

export function renderDebugPanel(
  enabled: boolean,
  params: Carousel3DParams,
  callbacks: DebugPanelCallbacks
): VNode {
  return h("div.debug-panel", {
    style: {
      position: "fixed",
      top: "60px",
      right: enabled ? "20px" : "-320px",
      width: "300px",
      maxHeight: "calc(100vh - 80px)",
      backgroundColor: "rgba(10, 10, 20, 0.95)",
      border: "1px solid var(--border-color)",
      borderRadius: "8px",
      padding: "16px",
      zIndex: "1000",
      transition: "right 0.3s cubic-bezier(0.4, 0, 0.2, 1)",
      overflowY: "auto",
      backdropFilter: "blur(10px)",
      boxShadow: "0 8px 32px rgba(0, 0, 0, 0.5)",
    },
  }, [
    h("div", {
      style: {
        display: "flex",
        justifyContent: "space-between",
        alignItems: "center",
        marginBottom: "16px",
        paddingBottom: "12px",
        borderBottom: "1px solid var(--border-color)",
      },
    }, [
      h("h3", {
        style: {
          fontSize: "0.875rem",
          fontWeight: "600",
          color: "var(--text-primary)",
          fontFamily: "var(--font-display)",
          letterSpacing: "0.1em",
          textTransform: "uppercase",
          margin: "0",
        },
      }, "3D Debug"),
      h("button", {
        style: {
          background: "none",
          border: "none",
          color: "var(--text-secondary)",
          cursor: "pointer",
          fontSize: "1.25rem",
          padding: "0",
          lineHeight: "1",
          transition: "color 0.2s",
        },
        on: {
          click: callbacks.onToggle,
        },
      }, "×"),
    ]),
    
    h("div.debug-controls", [
      ...sliderConfigs.map(config => 
        renderSlider(config, params[config.param] as number, callbacks)
      ),
      renderToggle("Show Debug Dots", "showDebugDots", params.showDebugDots, callbacks),
    ]),
    
    h("button", {
      style: {
        width: "100%",
        marginTop: "12px",
        padding: "10px",
        backgroundColor: "rgba(0, 229, 255, 0.1)",
        border: "1px solid var(--accent-blue)",
        borderRadius: "6px",
        color: "var(--accent-blue)",
        fontSize: "0.75rem",
        fontWeight: "600",
        fontFamily: "var(--font-display)",
        letterSpacing: "0.08em",
        textTransform: "uppercase",
        cursor: "pointer",
        transition: "all 0.2s",
      },
      on: {
        click: () => printParamsToConsole(params),
      },
    }, "📋 Print to Console"),
    
    h("div", {
      style: {
        marginTop: "12px",
        paddingTop: "12px",
        borderTop: "1px solid var(--border-color)",
        fontSize: "0.7rem",
        color: "var(--text-secondary)",
        fontFamily: "var(--font-mono)",
        lineHeight: "1.4",
      },
    }, "Adjust parameters to tweak the 3D perspective. Changes apply in real-time."),
  ]);
}

export function renderDebugToggleButton(
  enabled: boolean,
  callbacks: DebugPanelCallbacks
): VNode {
  return h("button.debug-toggle-btn", {
    style: {
      position: "fixed",
      top: "20px",
      right: "20px",
      width: "50px",
      height: "50px",
      borderRadius: "8px",
      backgroundColor: enabled ? "var(--accent-blue)" : "rgba(0, 229, 255, 0.3)",
      border: `2px solid ${enabled ? "var(--accent-blue)" : "var(--accent-blue)"}`,
      color: enabled ? "var(--bg-primary)" : "var(--text-primary)",
      fontSize: "0.75rem",
      fontWeight: "700",
      cursor: "pointer",
      zIndex: "1001",
      transition: "all 0.3s cubic-bezier(0.4, 0, 0.2, 1)",
      display: "flex",
      alignItems: "center",
      justifyContent: "center",
      backdropFilter: "blur(10px)",
      boxShadow: enabled ? "0 4px 16px rgba(0, 229, 255, 0.5)" : "0 2px 8px rgba(0, 229, 255, 0.3)",
      fontFamily: "var(--font-display)",
      letterSpacing: "0.05em",
    },
    on: {
      click: callbacks.onToggle,
    },
  }, "DEBUG");
}
