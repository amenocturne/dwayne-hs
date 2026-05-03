import { h } from "snabbdom/build/h.js";
import type { VNode } from "snabbdom/build/vnode.js";

let particleSystemInitialized = false;

function initParticleSystem(container: HTMLElement) {
  if (particleSystemInitialized) return;
  particleSystemInitialized = true;

  const HISTORY_SIZE = 400;

  // Create unique gradient ID for each particle
  let gradientId = 0;

  function createParticle() {
    const id = gradientId++;

    // Create SVG for curved trail
    const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
    svg.setAttribute("class", "light-trail-svg");
    svg.setAttribute("width", "100%");
    svg.setAttribute("height", "100%");
    svg.style.position = "absolute";
    svg.style.top = "0";
    svg.style.left = "0";
    svg.style.pointerEvents = "none";
    svg.style.overflow = "visible";

    // Gradient definition
    const defs = document.createElementNS("http://www.w3.org/2000/svg", "defs");
    const gradient = document.createElementNS("http://www.w3.org/2000/svg", "linearGradient");
    gradient.setAttribute("id", `trail-gradient-${id}`);
    gradient.setAttribute("gradientUnits", "userSpaceOnUse");

    const stop1 = document.createElementNS("http://www.w3.org/2000/svg", "stop");
    stop1.setAttribute("offset", "0%");
    stop1.setAttribute("stop-color", "rgba(0, 229, 255, 0)");

    const stop2 = document.createElementNS("http://www.w3.org/2000/svg", "stop");
    stop2.setAttribute("offset", "50%");
    stop2.setAttribute("stop-color", "rgba(0, 229, 255, 0.6)");

    const stop3 = document.createElementNS("http://www.w3.org/2000/svg", "stop");
    stop3.setAttribute("offset", "100%");
    stop3.setAttribute("stop-color", "rgba(255, 255, 255, 0.95)");

    gradient.appendChild(stop1);
    gradient.appendChild(stop2);
    gradient.appendChild(stop3);
    defs.appendChild(gradient);
    svg.appendChild(defs);

    // Path element
    const path = document.createElementNS("http://www.w3.org/2000/svg", "path");
    path.setAttribute("fill", "none");
    path.setAttribute("stroke", `url(#trail-gradient-${id})`);
    path.setAttribute("stroke-width", "8");
    path.setAttribute("stroke-linecap", "round");
    path.setAttribute("filter", "url(#glow)");
    svg.appendChild(path);

    // Glow filter
    const filter = document.createElementNS("http://www.w3.org/2000/svg", "filter");
    filter.setAttribute("id", "glow");
    filter.setAttribute("x", "-50%");
    filter.setAttribute("y", "-50%");
    filter.setAttribute("width", "200%");
    filter.setAttribute("height", "200%");
    const feGaussianBlur = document.createElementNS("http://www.w3.org/2000/svg", "feGaussianBlur");
    feGaussianBlur.setAttribute("stdDeviation", "4");
    feGaussianBlur.setAttribute("result", "coloredBlur");
    const feMerge = document.createElementNS("http://www.w3.org/2000/svg", "feMerge");
    const feMergeNode1 = document.createElementNS("http://www.w3.org/2000/svg", "feMergeNode");
    feMergeNode1.setAttribute("in", "coloredBlur");
    const feMergeNode2 = document.createElementNS("http://www.w3.org/2000/svg", "feMergeNode");
    feMergeNode2.setAttribute("in", "SourceGraphic");
    feMerge.appendChild(feMergeNode1);
    feMerge.appendChild(feMergeNode2);
    filter.appendChild(feGaussianBlur);
    filter.appendChild(feMerge);
    defs.appendChild(filter);

    container.appendChild(svg);

    // Random start position on left or right edge
    const startFromLeft = Math.random() > 0.5;
    let x = startFromLeft ? -10 : window.innerWidth + 10;
    let y = Math.random() * window.innerHeight * 0.6 + window.innerHeight * 0.1;

    // Easing parameters
    const screenWidth = window.innerWidth;
    const direction = startFromLeft ? 1 : -1;
    const maxVelX = 50;

    // Curve parameters
    const curveAmplitude = 8 + Math.random() * 12;
    const curvePhase = Math.random() * Math.PI * 2;
    const curveDirection = Math.random() > 0.5 ? 1 : -1;

    // Position history
    const history: { x: number; y: number }[] = [];

    let alive = true;
    let frameCount = 0;

    function peakInMiddle(t: number): number {
      return Math.sin(t * Math.PI);
    }

    function update() {
      if (!alive) return;
      frameCount++;

      // Calculate progress (0 to 1) across screen
      const progress = startFromLeft
        ? x / screenWidth
        : (screenWidth - x) / screenWidth;

      // Clamp progress between 0 and 1
      const clampedProgress = Math.max(0, Math.min(1, progress));

      // Ease velocity - moderate at edges, fast in middle
      const easedSpeed = 15 + peakInMiddle(clampedProgress) * maxVelX;
      const velX = easedSpeed * direction;

      // Smooth sweeping curve using sine wave
      const velY = Math.sin(curvePhase + frameCount * 0.15) * curveAmplitude * curveDirection;

      x += velX;
      y += velY;

      history.push({ x, y });
      if (history.length > HISTORY_SIZE) {
        history.shift();
      }

      // Build smooth curved path
      if (history.length >= 4) {
        let d = `M ${history[0].x} ${history[0].y}`;

        const tension = 0.3;
        for (let i = 0; i < history.length - 1; i++) {
          const p0 = history[Math.max(0, i - 1)];
          const p1 = history[i];
          const p2 = history[Math.min(history.length - 1, i + 1)];
          const p3 = history[Math.min(history.length - 1, i + 2)];

          const cp1x = p1.x + (p2.x - p0.x) * tension;
          const cp1y = p1.y + (p2.y - p0.y) * tension;
          const cp2x = p2.x - (p3.x - p1.x) * tension;
          const cp2y = p2.y - (p3.y - p1.y) * tension;

          d += ` C ${cp1x} ${cp1y}, ${cp2x} ${cp2y}, ${p2.x} ${p2.y}`;
        }

        path.setAttribute("d", d);

        // Update gradient direction
        const first = history[0];
        const last = history[history.length - 1];
        gradient.setAttribute("x1", String(first.x));
        gradient.setAttribute("y1", String(first.y));
        gradient.setAttribute("x2", String(last.x));
        gradient.setAttribute("y2", String(last.y));
      }

      const offScreen = startFromLeft
        ? x > window.innerWidth + 50
        : x < -50;

      if (offScreen) {
        alive = false;
        svg.style.transition = "opacity 2s ease-out";
        svg.style.opacity = "0";
        setTimeout(() => svg.remove(), 2100);
      } else {
        requestAnimationFrame(update);
      }
    }

    requestAnimationFrame(update);
  }

  // Spawn particles periodically (only when tab is visible)
  function spawnParticle() {
    if (!document.hidden) {
      createParticle();
    }
    // Next particle in 8-15 seconds
    const nextSpawn = 8000 + Math.random() * 7000;
    setTimeout(spawnParticle, nextSpawn);
  }

  // Clear all particles when tab becomes hidden
  document.addEventListener("visibilitychange", () => {
    if (document.hidden) {
      // Remove all existing trails when tab is hidden
      const trails = container.querySelectorAll(".light-trail-svg");
      trails.forEach((trail) => trail.remove());
    }
  });

  // Start spawning
  spawnParticle();
}

/**
 * Renders atmospheric effects for NFS Carbon garage vibes
 * Creates light particles that float across the screen
 */
export function renderAtmosphere(): VNode {
  return h(
    "div.atmosphere-container",
    {
      style: {
        position: "fixed",
        top: "0",
        left: "0",
        width: "100%",
        height: "100%",
        pointerEvents: "none",
        zIndex: "2",
        overflow: "hidden",
      },
      hook: {
        insert: (vnode) => {
          if (vnode.elm) {
            initParticleSystem(vnode.elm as HTMLElement);
          }
        },
      },
    },
    []
  );
}
