import puppeteer from 'puppeteer';
import fs from 'fs';

const modelsDir = process.argv[2];
if (!modelsDir) {
  console.error('Usage: node screenshot.mjs <modelsDir>');
  process.exit(1);
}
const outputDir = `./previews/${modelsDir}`;
const baseUrl = `http://localhost:9247/viewer.html`;

if (!fs.existsSync(outputDir)) fs.mkdirSync(outputDir);

const modelFiles = fs.readdirSync(modelsDir).filter(f => f.endsWith('.obj'));

const browser = await puppeteer.launch({executablePath: '/usr/bin/chromium', headless: false});
const page = await browser.newPage();
await page.setViewport({ width: 1024, height: 1024 });

for (let i = 0; i < modelFiles.length; i++) {
  const model = modelFiles[i];
  const modelPath = `${modelsDir}/${model}`;
  const texturePath = `${modelsDir}/textures/${model.replace('.obj', '.png')}`;
  const url = `${baseUrl}?model=${modelPath}&texture=${texturePath}`;

  console.log(`Rendering ${model}...`);
  await page.goto(url);
  await page.screenshot({ path: `${outputDir}/${model.replace('.obj', '.png')}` });
  const screenshotPath = `${outputDir}/${model.replace('.obj', '.png')}`;
  const { execSync } = await import('child_process');
  execSync(`mogrify -trim "${screenshotPath}"`);
}

await browser.close();
console.log('All previews saved.');
