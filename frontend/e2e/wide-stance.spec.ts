import { expect, test } from '@playwright/test'

test('Wide Stance distinguishes discarding a card from choosing the top card', async ({ page }) => {
  await page.goto('/#/cards/22008')
  await page.evaluate(async () => {
    const load = (path: string) => import(/* @vite-ignore */ path)
    const { createApp, h, ref } = await load('/node_modules/.vite/deps/vue.js')
    const { createI18n } = await load('/node_modules/.vite/deps/vue-i18n.js')
    const { default: CardPick } = await load('/src/components/CardPick.vue')
    const { searchedCards } = await load('/src/composables/cardSearch.ts')
    const { loadLocaleMessages } = await load('/src/locales/messages.ts')
    const { messages } = await loadLocaleMessages('en')
    const focusedCards = [
      { id: 'assault', code: '01150', name: 'Assault' },
      { id: 'advance', code: '01149', name: 'Advance' },
      { id: 'gang-up', code: '01153', name: 'Gang-Up' },
    ]
    const discarding = ref(true)
    const host = document.createElement('div')
    document.body.replaceChildren(host)
    host.id = 'wide-stance-host'
    createApp({ setup: () => () => {
      const question = {
        prompt: discarding.value ? '$label.wideStanceDiscard' : '$label.wideStanceOrder',
        choices: focusedCards.filter(card => discarding.value || card.id !== 'advance').map((card, index) => ({
          index, target: card.id, code: card.code,
          kind: discarding.value ? 'discard' : 'card',
          label: discarding.value ? '$label.discard' : '$label.putOnTopOfEncounterDeck',
        })),
      }
      return h(CardPick, {
        ...question, searched: searchedCards({ focusedCards }, question), canChoose: true,
        onChoose: (index: number) => {
          host.dataset.chosen = String(index)
          discarding.value = false
        },
      })
    } }).use(createI18n({ legacy: false, locale: 'en', messages: { en: messages } })).mount(host)
  })
  const dialog = page.getByRole('dialog')
  await expect(dialog).toHaveAccessibleName('Wide Stance: discard 1 card, then return the others to the encounter deck')
  await expect(dialog.locator('.card .action')).toHaveText(['Discard', 'Discard', 'Discard'])
  await dialog.locator('.card').nth(1).click()
  await expect(page.locator('#wide-stance-host')).toHaveAttribute('data-chosen', '1')
  await expect(dialog).toHaveAccessibleName('Wide Stance: choose the top card; the other card goes beneath it')
  await expect(dialog.locator('.card:not(:disabled) .action')).toHaveText([
    'Put it on top of the encounter deck', 'Put it on top of the encounter deck',
  ])
  await expect(dialog.locator('.card:disabled')).toHaveCount(1)
})
