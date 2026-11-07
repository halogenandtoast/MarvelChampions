import { expect, test } from '@playwright/test'

test('a named action animates to its name on hover and keyboard focus, then reverts', async ({ page }) => {
  await page.goto('/#/cards/59001b')
  await page.evaluate(async () => {
    const load = (path: string) => import(/* @vite-ignore */ path)
    const { createApp, h } = await load('/node_modules/.vite/deps/vue.js')
    const { default: ActionLabel } = await load('/src/components/ActionLabel.vue')
    const host = document.createElement('div')
    host.style.cssText = 'position:fixed;left:400px;top:200px;width:130px'
    document.body.replaceChildren(host)
    createApp({ setup: () => () => [
      h('button', { style: 'width:130px' }, [h(ActionLabel, { text: 'Action', name: 'New Labors of Hercules' })]),
      h('button', [h(ActionLabel, { text: 'Change form' })]),
    ] }).mount(host)
  })
  const action = page.getByRole('button', { name: 'New Labors of Hercules', exact: true })
  const short = action.locator('.short')
  const named = action.locator('.named')
  await expect(short).toHaveCSS('opacity', '1')
  await expect(named).toHaveCSS('opacity', '0')
  const initialHeight = (await action.boundingBox())!.height
  await action.hover()
  await expect(named).toHaveCSS('opacity', '1')
  await expect(short).toHaveCSS('opacity', '0')
  await expect.poll(async () => (await action.boundingBox())!.height > initialHeight).toBe(true)
  await page.mouse.move(20, 20)
  await expect(short).toHaveCSS('opacity', '1')
  await expect(named).toHaveCSS('opacity', '0')
  await action.focus()
  await expect(named).toHaveCSS('opacity', '1')
  const changeForm = page.getByRole('button', { name: 'Change form' })
  await changeForm.focus()
  await expect(short).toHaveCSS('opacity', '1')
  await expect(changeForm.locator('.action-label')).toHaveCount(0)
  await expect(action).not.toHaveAttribute('title')
  await expect(page.getByRole('tooltip')).toHaveCount(0)
})
