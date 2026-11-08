import { expect, test, type Page } from '@playwright/test'

const mount = async (page: Page, picks: number | null) => {
  await page.goto('/#/cards/01001a')
  await page.evaluate(async picks => {
    const load = (path: string) => import(/* @vite-ignore */ path)
    const { createApp, h, reactive } = await load('/node_modules/.vite/deps/vue.js')
    const { createI18n } = await load('/node_modules/.vite/deps/vue-i18n.js')
    const { default: Pile } = await load('/src/components/Pile.vue')
    const { default: CardOverlay } = await load('/src/components/CardOverlay.vue')
    const { provideChoices } = await load('/src/composables/useChoices.ts')
    const { withViewTransition, transitionsOn } = await load('/src/composables/useViewTransition.ts')
    const { riffle } = await load('/src/composables/dealing.ts')
    const { loadLocaleMessages } = await load('/src/locales/messages.ts')
    const { messages } = await loadLocaleMessages('en')
    const cards = [{ id: 'first', code: '56035', name: 'Imitation Shape' }, { id: 'second', code: '56034', name: 'Armored Shape' }, { id: 'third', code: '56033', name: 'Hulk Shape' }]
    const state = reactive({ cards, remaining: picks, updates: 0, choices: picks === null ? [] : cards.map((card, index) => ({ index, kind: 'target', target: card.id })) })
    const host = document.createElement('div')
    host.id = 'pile-host'
    host.style.cssText = '--card-width:100px;display:flex;gap:50px;padding:100px'
    document.body.replaceChildren(host)
    const animateUpdate = async () => {
      host.dataset.transitionsAllowed = String(transitionsOn())
      await riffle('deck-target')
      // Model intermediate snapshots before the final answered question arrives.
      await withViewTransition(() => { state.updates++ })
      await withViewTransition(() => { state.updates++ })
    }
    Object.assign(window, { animatePileUpdate: animateUpdate })
    createApp({ setup() {
      provideChoices(() => state.choices, async (index: number) => {
        host.dataset.openAtAnswer = String(document.querySelector<HTMLDialogElement>('.pile-dialog[open]') !== null)
        await animateUpdate()
        const chosen = state.choices.find((choice: any) => choice.index === index)?.target
        await withViewTransition(() => {
          state.cards = state.cards.filter((card: any) => card.id !== chosen)
          state.choices = state.choices.filter((choice: any) => choice.index !== index)
          if (state.remaining !== null) state.remaining--
        })
        host.dataset.finished = String(index)
      }, () => null, () => ({}), () => state.remaining)
      return () => [h(Pile, { label: 'Discard', cards: state.cards, pile: 'discard-test' }), h(Pile, { label: 'Deck', back: 'player', count: 8, pile: 'deck-target' }), h('span', String(state.updates)), h(CardOverlay)]
    } }).use(createI18n({ legacy: false, locale: 'en', messages: { en: messages } })).mount(host)
  }, picks)
}

for (const picks of [1, 2]) {
  test(`pile selection closes before the final shuffle and stays closed through transitions (${picks} picks)`, async ({ page }) => {
    await mount(page, picks)
    await page.getByRole('button', { name: 'Discard', exact: true }).click()
    const dialog = page.getByRole('dialog')
    await expect(dialog).toHaveAttribute('data-mode', 'pick')
    await expect(dialog).toHaveAttribute('data-remaining-picks', String(picks))
    await expect(dialog.locator('.pick-instruction')).toHaveText(`Choose ${picks} ${picks === 1 ? 'card' : 'cards'}`)
    if (picks === 2) {
      await dialog.locator('[data-card-id="first"] .chip').click()
      await expect(page.locator('#pile-host')).toHaveAttribute('data-open-at-answer', 'true')
      await expect(dialog).toBeVisible()
      await expect(page.locator('#pile-host')).toHaveAttribute('data-finished', '0')
      await expect(dialog).toHaveAttribute('data-remaining-picks', '1')
    }
    const finalId = picks === 1 ? 'first' : 'second'
    await dialog.locator(`[data-card-id="${finalId}"] .chip`).click()
    await expect(page.locator('#pile-host')).toHaveAttribute('data-open-at-answer', 'false')
    await expect(dialog).toHaveCount(0)
    await expect(page.locator('#pile-host')).toHaveAttribute('data-finished', String(picks - 1))
    await expect(page.locator('.pile-dialog[open]')).toHaveCount(0)
    await expect(page.locator('.card-overlay')).toHaveCount(0)
  })
}

test('browsing a pile stays open through board transitions without becoming a selection', async ({ page }) => {
  await mount(page, null)
  await page.getByRole('button', { name: 'Discard', exact: true }).click()
  const dialog = page.getByRole('dialog')
  await expect(dialog).toHaveAttribute('data-mode', 'browse')
  await expect(dialog.locator('.pick-instruction')).toHaveCount(0)
  await expect(dialog).toHaveCSS('view-transition-name', 'pile-browser')
  await dialog.locator('[data-card-id="first"]').click()
  await page.evaluate(() => (window as any).animatePileUpdate())
  await expect(dialog).toBeVisible()
  await expect(page.locator('#pile-host')).toHaveAttribute('data-transitions-allowed', 'false')
  await dialog.getByRole('button', { name: 'Close', exact: true }).click()
  await expect(dialog).toHaveCount(0)
})
