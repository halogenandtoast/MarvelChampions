import { expect, test } from '@playwright/test'

test('the picker stays open between picks and dismisses immediately on the final pick', async ({ page }) => {
  await page.goto('/#/cards/01001a')
  await page.evaluate(async () => {
    const load = (path: string) => import(/* @vite-ignore */ path)
    const { createApp, h, reactive } = await load('/node_modules/.vite/deps/vue.js')
    const { createI18n } = await load('/node_modules/.vite/deps/vue-i18n.js')
    const { default: CardPick } = await load('/src/components/CardPick.vue')
    const { default: CardOverlay } = await load('/src/components/CardOverlay.vue')
    const { loadLocaleMessages } = await load('/src/locales/messages.ts')
    const { messages } = await loadLocaleMessages('en')
    const state = reactive({ open: true, remaining: 2, answers: [] as number[], choices: [
      { index: 0, kind: 'card', code: '01088' },
      { index: 1, kind: 'card', code: '01089' },
      { index: 2, kind: 'card', code: '01090' },
    ] })
    const host = document.createElement('div')
    document.body.replaceChildren(host)
    host.id = 'picker-host'
    createApp({ setup: () => () => [state.open ? h(CardPick, {
      choices: state.choices, remainingPicks: state.remaining, canChoose: true,
      onChoose: (index: number) => {
        state.answers.push(index)
        host.dataset.answers = state.answers.join(',')
        if (state.remaining > 1) {
          state.remaining--
          state.choices = state.choices.filter(choice => choice.index !== index)
        }
        // Leave the final question unchanged, as while waiting for the server.
      },
      onClose: () => { state.open = false },
    }) : null, h(CardOverlay)] })
      .use(createI18n({ legacy: false, locale: 'en', messages: { en: messages } })).mount(host)
  })
  const picker = page.getByRole('dialog')
  await expect(picker.locator('.cards button')).toHaveCount(3)
  await picker.locator('.cards button').first().click()
  await expect(page.locator('#picker-host')).toHaveAttribute('data-answers', '0')
  await expect(picker).toBeVisible()
  await expect(picker.locator('.cards button')).toHaveCount(2)
  await picker.locator('.cards button').first().hover()
  await expect(page.locator('.card-overlay')).toBeVisible()
  await picker.locator('.cards button').first().click()
  await expect(page.locator('#picker-host')).toHaveAttribute('data-answers', '0,1')
  await expect(picker).toHaveCount(0)
  await expect(page.locator('.card-overlay')).toHaveCount(0)
})
