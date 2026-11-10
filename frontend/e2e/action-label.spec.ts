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

test('Child Prodigy combines alternative payments into one action', async ({ page }) => {
  await page.goto('/#/cards/29002b')
  await page.evaluate(async () => {
    const load = (path: string) => import(/* @vite-ignore */ path)
    const { createApp, h } = await load('/node_modules/.vite/deps/vue.js')
    const { createI18n } = await load('/node_modules/.vite/deps/vue-i18n.js')
    const { default: BoardCard } = await load('/src/components/BoardCard.vue')
    const source = await (await fetch('/src/components/BoardCard.vue')).text()
    const path = source.match(/from ["']([^"']*\/composables\/useChoices\.ts[^"']*)["']/)?.[1]
    if (!path) throw new Error('Choices import not found')
    const { provideChoices } = await load(path)
    const { loadLocaleMessages } = await load('/src/locales/messages.ts')
    const { messages } = await loadLocaleMessages('en')
    const host = document.createElement('div')
    host.id = 'riri-actions'
    host.style.cssText = '--card-width:180px;position:fixed;left:300px;top:150px'
    document.body.replaceChildren(host)
    createApp({ setup() {
      provideChoices(() => [
        { index: 1, kind: 'ability', target: 'riri', label: 'Child Prodigy', abilityType: 'Action', paymentLabel: '$label.childProdigyMentalPayment' },
        { index: 2, kind: 'ability', target: 'riri', label: 'Child Prodigy', abilityType: 'Action', paymentLabel: '$label.childProdigyTwoResourcesPayment' },
        { index: 3, kind: 'ability', target: 'riri', label: '$label.recover', abilityType: 'Basic' },
      ], (index: number) => { host.dataset.chosen = String(index) })
      return () => h(BoardCard, { id: 'riri', code: '29002b' })
    } }).use(createI18n({ legacy: false, locale: 'en', messages: { en: messages } })).mount(host)
  })
  const action = page.locator('#riri-actions .chips').getByRole('button', { name: 'Child Prodigy', exact: true })
  await expect(action).toHaveCount(1)
  await expect(page.locator('#riri-actions .chips button')).toHaveCount(2)
  await action.click()
  await expect(page.getByRole('menuitem')).toHaveCount(2)
  await page.getByRole('menuitem', { name: /Spend 1 mental resource/ }).click()
  await expect(page.locator('#riri-actions')).toHaveAttribute('data-chosen', '1')
  await action.click()
  await page.getByRole('menuitem', { name: /Spend 2 resources of any type/ }).click()
  await expect(page.locator('#riri-actions')).toHaveAttribute('data-chosen', '2')
})
