import { expect, test } from '@playwright/test'

test('relevant discards sit to the right of the resolving card and highlight the count in progress', async ({ page }) => {
  await page.goto('/#/cards/15005')
  await page.evaluate(async () => {
    const load = (path: string) => import(/* @vite-ignore */ path)
    const { createApp, h, ref } = await load('/node_modules/.vite/deps/vue.js')
    const { createI18n } = await load('/node_modules/.vite/deps/vue-i18n.js')
    const { default: River } = await load('/src/components/River.vue')
    const source = await (await fetch('/src/components/BoardCard.vue')).text()
    const choicesPath = source.match(/from ["']([^"']*\/composables\/useChoices\.ts[^"']*)["']/)?.[1]
    if (!choicesPath) throw new Error('Choices module not found')
    const { provideChoices } = await load(choicesPath)
    const { loadLocaleMessages } = await load('/src/locales/messages.ts')
    const { messages } = await loadLocaleMessages('en')
    const counted = ref('first-discard')
    const discards = ref([{ id: 'first-discard', code: '01107' }, { id: 'second-discard', code: '01106' }])
    const resolving = ref([{ id: 'decay', code: '15005' }])
    const host = document.createElement('div')
    host.style.cssText = '--card-width:124px;max-width:1400px;margin:30px'
    document.body.replaceChildren(host)
    createApp({ setup() {
      provideChoices(() => [], () => {}, () => counted.value)
      return () => h(River, { cards: [], resolvingEvents: resolving.value, discardedCards: discards.value, elsewhere: new Set() })
    } }).use(createI18n({ legacy: false, locale: 'en', messages: { en: messages } })).mount(host)
    Object.assign(window, {
      countSecondDiscard: () => { counted.value = 'second-discard' },
      finishDiscardEffect: () => { discards.value = []; resolving.value = [] },
    })
  })
  const resolving = page.getByRole('region', { name: 'Resolving', exact: true })
  const discarded = page.getByRole('region', { name: 'Discarded', exact: true })
  await expect(discarded.locator('.card')).toHaveCount(2)
  const left = (await resolving.boundingBox())!
  const right = (await discarded.boundingBox())!
  expect(right.x).toBeGreaterThanOrEqual(left.x + left.width)
  await expect(page.locator('[data-card-id="first-discard"]')).toHaveClass(/source/)
  await expect(page.locator('[data-card-id="second-discard"]')).not.toHaveClass(/source/)
  await page.evaluate(() => (window as any).countSecondDiscard())
  await expect(page.locator('[data-card-id="second-discard"]')).toHaveClass(/source/)
  await expect(page.locator('[data-card-id="first-discard"]')).not.toHaveClass(/source/)
  await page.evaluate(() => (window as any).finishDiscardEffect())
  await expect(discarded).toHaveCount(0)
})
