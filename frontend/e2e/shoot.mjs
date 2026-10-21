// Screenshots of every page, for looking the design over: node e2e/shoot.mjs [outdir] [path...]
import { chromium } from '@playwright/test'

const base = process.env.BASE ?? 'http://localhost:5180'
const out = process.argv[2] ?? 'shots'
const paths = process.argv.slice(3)
const login = async () => {
  const r = await fetch(`${base}/api/authenticate`, { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify({ login: 'tester', password: 'password123' }) })
  return r.json()
}
const session = await login()
const browser = await chromium.launch()
const page = await browser.newPage({ viewport: { width: Number(process.env.W ?? 1440), height: Number(process.env.H ?? 900) } })
page.on('console', (m) => (m.type() === 'error' || m.type() === 'warning') && console.log(`[${m.type()}]`, m.text().slice(0, 300)))
page.on('pageerror', (e) => console.log('[pageerror]', e.message))
await page.goto(base)
await page.evaluate((s) => localStorage.setItem('marvel-session', JSON.stringify(s)), session)
await page.reload()
for (const path of paths) {
  await page.goto(`${base}/#${path}`)
  await page.reload()
  await page.waitForTimeout(Number(process.env.WAIT ?? 1500))
  // CLICK names something to press first, as `text=All seats`
  if (process.env.CLICK) {
    await page.locator(process.env.CLICK).first().click()
    await page.waitForTimeout(600)
  }
  const name = path.replace(/[^a-z0-9]+/gi, '_') || 'home'
  await page.screenshot({ path: `${out}/${name}.png`, fullPage: process.env.FULL === '1' })
  console.log('shot', path)
}
await browser.close()
