import { expect, test } from '@playwright/test'

test('preview closes when its card disappears without pointerleave', async ({ page }) => {
  await page.goto('/#/cards/01001a')
  const visible = await page.evaluate(async () => {
    const load = (path: string) => import(/* @vite-ignore */ path)
    const { useCardOverlay } = await load('/src/composables/useCardOverlay.ts')
    const overlay = useCardOverlay()
    const container = document.createElement('div')
    const anchor = document.createElement('div')
    anchor.style.cssText = 'position:fixed;left:400px;top:300px;width:100px;height:140px'
    container.append(anchor)
    document.body.append(container)
    overlay.show({ code: '01001a' }, anchor)
    return overlay.visible.value
  })
  expect(visible).toBe(true)
  // Remove its entire zone, without moving the pointer or scrolling.
  await page.evaluate(() => document.querySelector('div[style*="left: 400px"]')!.parentElement!.remove())
  await expect.poll(() => page.evaluate(async () => {
    const load = (path: string) => import(/* @vite-ignore */ path)
    const { useCardOverlay } = await load('/src/composables/useCardOverlay.ts')
    return useCardOverlay().visible.value
  })).toBe(false)
})
