import { chromium } from "playwright";
import sharp from "sharp";
import fs from "node:fs/promises";
import path from "node:path";

const BASE_URL = "http://127.0.0.1:5500/demo.html?full";
const FROM = 1;
const TO = 29;

const OUT_DIR = "screenshots";

// 1920x1080 gives normal Full HD slides.
// Set DEVICE_SCALE_FACTOR to 2 for 3840x2160 output.
const VIEWPORT = { width: 1920, height: 1080 };
const DEVICE_SCALE_FACTOR = 1;

await fs.mkdir(OUT_DIR, { recursive: true });

const browser = await chromium.launch();

const context = await browser.newContext({
  viewport: VIEWPORT,
  deviceScaleFactor: DEVICE_SCALE_FACTOR,
});

const page = await context.newPage();

for (let i = FROM; i <= TO; i++) {
  const url = `${BASE_URL}#${i}`;
  const outPath = path.join(OUT_DIR, `slide-${String(i).padStart(3, "0")}.webp`);

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

await browser.close();