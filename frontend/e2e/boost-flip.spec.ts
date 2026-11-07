import { expect, test } from '@playwright/test'

test('a boost flips in place and the river offers an explicit resolution button', async ({ page }) => {
  await page.goto('/#/cards/01001a')
  await page.evaluate(async () => {
    const load = (path: string) => import(/* @vite-ignore */ path)
    const { createApp, h, ref, nextTick } = await load('/node_modules/.vite/deps/vue.js')
    const { createI18n } = await load('/node_modules/.vite/deps/vue-i18n.js')
    const { default: River } = await load('/src/components/River.vue')
    const componentSource = await (await fetch('/src/components/BoardCard.vue')).text()
    const choicesPath = componentSource.match(/from ["']([^"']*\/composables\/useChoices\.ts[^"']*)["']/)?.[1]
    if (!choicesPath) throw new Error('BoardCard choices import not found')
    const { provideChoices } = await load(choicesPath)
    const { loadLocaleMessages } = await load('/src/locales/messages.ts')
    const { messages } = await loadLocaleMessages('en')
    const cards = ref([{ id: 'boost-flip-test', code: null }])
    const actions = ref([])
    const host = document.createElement('div')
    host.id = 'boost-host'
    host.style.setProperty('--card-width', '124px')
    document.body.replaceChildren(host)
    createApp({ setup() {
      provideChoices(() => [{ index: 0, kind: 'target', target: 'boost-flip-test', label: '$label.revealBoostCards' }], () => {}, () => 'boost-flip-test')
      return () => h(River, {
      cards: cards.value, actions: actions.value, elsewhere: new Set(),
      attacker: { id: 'villain-test', code: '27113', name: 'Venom Goblin', attack: 2 },
      onChoose: (index: number) => { host.dataset.chosen = String(index) },
    }) } }).use(createI18n({ legacy: false, locale: 'en', messages: { en: messages } })).mount(host)
    Object.assign(window, { flipBoost: async () => {
      cards.value = [{ id: 'boost-flip-test', code: '01186' }]
      await nextTick()
    }, resolveBoost: () => {
      actions.value = [{ index: 7, kind: 'label', label: '$label.placeActivationThreat count=i:4' }]
    } })
  })
  const card = page.locator('[data-card-id="boost-flip-test"]')
  await expect(card.locator('.reveal-back')).toHaveAttribute('src', /back-encounter/)
  await expect(card.getByRole('button', { name: 'Reveal', exact: true })).toBeVisible()
  await expect(card).toHaveClass(/active/)
  const before = (await card.boundingBox())!
  await page.evaluate(() => (window as any).flipBoost())
  await expect(card.locator('.reveal-front')).toHaveAttribute('src', /01186/)
  await expect(card.getByRole('button', { name: 'Reveal', exact: true })).toHaveCount(0)
  await expect(card).not.toHaveClass(/active|source/)
  expect(await card.locator('.reveal-flip').evaluate(el => el.getAnimations().some(animation => animation instanceof CSSTransition && animation.transitionProperty === 'transform'))).toBe(true)
  await expect.poll(() => card.locator('.reveal-flip').evaluate(el => new DOMMatrix(getComputedStyle(el).transform).m11)).toBeLessThan(-0.99)
  const after = (await card.boundingBox())!
  expect(after.x).toBeCloseTo(before.x, 1)
  expect(after.y).toBeCloseTo(before.y, 1)
  expect(after.width).toBeCloseTo(before.width, 1)
  expect(after.height).toBeCloseTo(before.height, 1)
  await page.evaluate(() => (window as any).resolveBoost())
  const place = page.getByRole('button', { name: 'Place 4 threat', exact: true })
  await expect(page.locator('.enemy-card')).toContainText('Place 4 threat')
  const villain = (await page.locator('[data-card-id="villain-test"]').boundingBox())!
  expect((await place.boundingBox())!.y).toBeGreaterThanOrEqual(villain.y + villain.height)
  await place.click()
  await expect(page.locator('#boost-host')).toHaveAttribute('data-chosen', '7')
})
