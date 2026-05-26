import { chromium } from "playwright";
import sharp from "sharp";
import fs from "node:fs/promises";
import path from "node:path";

const HTML_PATH = "shower.js/demo.html";
const BASE_URL = "http://127.0.0.1:5500/demo.html?full";
const OUT_DIR = "screenshots";

// 1920x1080 gives normal Full HD slides.
// Set DEVICE_SCALE_FACTOR to 2 for 3840x2160 output.
const VIEWPORT = { width: 1920, height: 1080 };
const DEVICE_SCALE_FACTOR = 1;

function extractSlideIds(html) {
  const sectionStartRe = /<section\b(?=[^>]*\bclass\s*=\s*["'][^"']*\bslide\b[^"']*["'])[^>]*>/gi;
  const slides = [];

  for (const match of html.matchAll(sectionStartRe)) {
    const tag = match[0];
    const idMatch = tag.match(/\bid\s*=\s*["']([^"']+)["']/i);

    if (!idMatch) {
      throw new Error(
        `Found slide ${slides.length + 1}, but it has no id. ` +
        `Add a slug, for example: <section id="my-slide" class="slide">`
      );
    }

    slides.push(idMatch[1]);
  }

  if (slides.length === 0) {
    throw new Error(`No <section class="slide"> elements found in ${HTML_PATH}.`);
  }

  const duplicates = slides.filter((id, index) => slides.indexOf(id) !== index);
  if (duplicates.length > 0) {
    throw new Error(`Duplicate slide id(s): ${[...new Set(duplicates)].join(", ")}`);
  }

  return slides;
}

function safeFileName(slug) {
  return slug
    .normalize("NFKD")
    .replace(/[\u0300-\u036f]/g, "")
    .replace(/[^a-zA-Z0-9._-]+/g, "-")
    .replace(/^-+|-+$/g, "")
    .toLowerCase();
}

await fs.mkdir(OUT_DIR, { recursive: true });

const html = await fs.readFile(HTML_PATH, "utf8");
const slideIds = extractSlideIds(html);

console.log(`Found ${slideIds.length} slides in ${HTML_PATH}.`);

const browser = await chromium.launch();

try {
  const context = await browser.newContext({
    viewport: VIEWPORT,
    deviceScaleFactor: DEVICE_SCALE_FACTOR,
  });

  const page = await context.newPage();

  for (const slideId of slideIds) {
    const url = `${BASE_URL}#${encodeURIComponent(slideId)}`;
    const fileName = `${safeFileName(slideId)}.webp`;
    const outPath = path.join(OUT_DIR, fileName);

    console.log(`Capturing ${url} -> ${outPath}`);

    await page.goto(url, {
      waitUntil: "networkidle",
    });

    // Helpful if the page changes layout shortly after loading.
    await page.waitForTimeout(150);

    const pngBuffer = await page.screenshot({
      fullPage: false,
      animations: "disabled",
      scale: "device",
    });

    await sharp(pngBuffer)
      .webp({
        quality: 95,
        effort: 6,
        smartSubsample: true,
        preset: "text",
      })
      .toFile(outPath);
  }
} finally {
  await browser.close();
}