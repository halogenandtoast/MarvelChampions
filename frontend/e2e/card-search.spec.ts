import { expect, test } from '@playwright/test'

test('Sense deck choices use the card selection modal and preserve the selected answer', async ({ page }) => {
  await page.goto('/#/cards/01001a')
  await page.evaluate(async () => {
    const load = (path: string) => import(/* @vite-ignore */ path)
    const { createApp, h } = await load('/node_modules/.vite/deps/vue.js')
    const { createI18n } = await load('/node_modules/.vite/deps/vue-i18n.js')
    const { default: CardPick } = await load('/src/components/CardPick.vue')
    const { searchedCards } = await load('/src/composables/cardSearch.ts')
    const { loadLocaleMessages } = await load('/src/locales/messages.ts')
    const { messages } = await loadLocaleMessages('en')
    const senseDeck = [
      { id: 'acute', code: '60002', name: 'Acute Tactility' },
      { id: 'taste', code: '60006', name: 'Superior Taste' },
    ]
    const question = { searched: [], choices: [
      { index: 0, kind: 'label', label: '$label.done' },
      { index: 1, kind: 'target', target: 'taste', label: null },
    ] }
    const game = { players: [{ senseDeck }], focusedCards: [], question: { player: question } }
    const host = document.createElement('div')
    document.body.replaceChildren(host)
    host.id = 'sense-host'
    createApp({ setup: () => () => h(CardPick, {
      choices: question.choices, searched: searchedCards(game, question), canChoose: true,
      onChoose: (index: number) => { host.dataset.chosen = String(index) },
    }) }).use(createI18n({ legacy: false, locale: 'en', messages: { en: messages } })).mount(host)
  })
  const dialog = page.getByRole('dialog')
  await expect(dialog.getByRole('heading', { name: 'Sense deck', exact: true })).toBeVisible()
  await expect(dialog.locator('.card')).toHaveCount(2)
  await expect(dialog.locator('.card').first()).toBeDisabled()
  await expect(dialog.getByRole('button', { name: 'Done', exact: true })).toBeVisible()
  await dialog.locator('.card').last().click()
  await expect(page.locator('#sense-host')).toHaveAttribute('data-chosen', '1')
})

for (const noMatches of [false, true]) {
  for (const viewport of [{ width: 1280, height: 720 }, { width: 390, height: 667 }]) {
    test(`${noMatches ? 'unmatched' : 'focused'} search scrolls within the modal at ${viewport.width}px`, async ({ page }) => {
      await page.setViewportSize(viewport)
      await page.goto('/#/cards/01001a')
      const ids = await page.evaluate(async (noMatches) => {
        const load = (path: string) => import(/* @vite-ignore */ path)
        const { createApp, h } = await load('/node_modules/.vite/deps/vue.js')
        const { createI18n } = await load('/node_modules/.vite/deps/vue-i18n.js')
        const { default: CardPick } = await load('/src/components/CardPick.vue')
        const { searchedCards, searchCardIds } = await load('/src/composables/cardSearch.ts')
        const { loadLocaleMessages } = await load('/src/locales/messages.ts')
        const { messages } = await loadLocaleMessages('en')
        const focusedCards = Array.from({ length: 40 }, (_, i) => ({ id: `card-${i}`, code: '01001a', name: 'Spider-Man' }))
        const question = { choices: noMatches
          ? [{ index: 0, kind: 'label', label: '$label.noMatchingCardsFound' }]
          : [{ index: 0, kind: 'target', target: 'card-39', code: null }], searched: [] }
        const game = { focusedCards, question: { player: question } }
        const host = document.createElement('div')
        document.body.replaceChildren(host)
        host.id = 'search-host'
        createApp({ setup: () => () => h(CardPick, {
          choices: question.choices, searched: searchedCards(game, question), canChoose: true,
          onChoose: (index: number) => { host.dataset.chosen = String(index) },
        }) }).use(createI18n({ legacy: false, locale: 'en', messages: { en: messages } })).mount(host)
        return [...searchCardIds(game)]
      }, noMatches)
      expect(ids).toHaveLength(40)
      const dialog = page.getByRole('dialog')
      await expect(dialog).toBeVisible()
      const bounds = (await dialog.boundingBox())!
      expect(bounds.x).toBeGreaterThanOrEqual(0)
      expect(bounds.y).toBeGreaterThanOrEqual(0)
      expect(bounds.x + bounds.width).toBeLessThanOrEqual(viewport.width)
      expect(bounds.y + bounds.height).toBeLessThanOrEqual(viewport.height)
      expect(await dialog.locator('.pick-content').evaluate((el) => el.scrollHeight > el.clientHeight)).toBe(true)
      const last = dialog.locator('.card').last()
      await last.scrollIntoViewIfNeeded()
      await expect(dialog.getByRole('button', { name: 'Close', exact: true })).toBeInViewport()
      const scrollTop = await dialog.locator('.pick-content').evaluate((el) => el.scrollTop)
      await last.hover()
      await expect.poll(() => dialog.locator('.pick-content').evaluate((el) => el.scrollTop)).toBe(scrollTop)
      if (noMatches) {
        await expect(dialog.locator('.card:disabled')).toHaveCount(40)
        await dialog.getByRole('button', { name: 'No matching cards found', exact: true }).click()
      } else {
        await last.click()
      }
      await expect(page.locator('#search-host')).toHaveAttribute('data-chosen', '0')
    })
  }
}


test('a revealed encounter card is not a deck search', async ({ page }) => {
  await page.goto('/#/cards/01001a')
  const result = await page.evaluate(async () => {
    const load = (path: string) => import(/* @vite-ignore */ path)
    const { searchedCards, searchCardIds } = await load('/src/composables/cardSearch.ts')
    const question = {
      source: 'gang-up', prompt: '$label.resolveWhenRevealed', searched: [],
      choices: [{ index: 0, kind: 'target', target: 'gang-up', label: '$label.resolveWhenRevealed' }],
    }
    const game = { focusedCards: [{ id: 'gang-up', code: '01189', name: 'Gang-Up' }], question: { player: question } }
    return { groups: searchedCards(game, question), ids: [...searchCardIds(game)] }
  })
  expect(result).toEqual({ groups: [], ids: [] })
})
