import { expect, test } from '@playwright/test'

test('Black Panther names and previews his known tucked event', async ({ page }) => {
  await page.goto('/#/cards/23012')
  await page.evaluate(async () => {
    const load = (path: string) => import(/* @vite-ignore */ path)
    const { createApp, h } = await load('/node_modules/.vite/deps/vue.js')
    const { createI18n } = await load('/node_modules/.vite/deps/vue-i18n.js')
    const { default: PlayerArea } = await load('/src/components/PlayerArea.vue')
    const { default: CardOverlay } = await load('/src/components/CardOverlay.vue')
    const { loadLocaleMessages } = await load('/src/locales/messages.ts')
    const { messages } = await loadLocaleMessages('en')
    const host = document.createElement('div')
    host.style.setProperty('--card-width', '124px')
    document.body.replaceChildren(host)
    const player = {
      id: 'hero', code: '23001a', name: 'War Machine', faces: ['23001a', '23001b'],
      form: 'hero', hitPoints: 10, damage: 0, hand: [], discard: [], deckSize: 20,
      supports: [], upgrades: [], allyLimit: 3,
      allies: [{ id: 'panther', code: '23012', name: 'Black Panther', hitPoints: 4, damage: 0,
        upgrades: [], attachments: [], facedownCards: 1,
        heldCards: [{ id: 'event', code: '23019', name: 'Go Down Swinging' }],
      }],
    }
    createApp({ render: () => [h(PlayerArea, { player }), h(CardOverlay)] })
      .use(createI18n({ legacy: false, locale: 'en', messages: { en: messages } })).mount(host)
  })
  const tucked = page.locator('.ally .attached li')
  await expect(tucked).toHaveCount(1)
  await expect(tucked).toHaveText('Go Down Swinging')
  await tucked.click()
  await expect(page.locator('.card-overlay img').first()).toHaveAttribute('src', /23019/)
})
