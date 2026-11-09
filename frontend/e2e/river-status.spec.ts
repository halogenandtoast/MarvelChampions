import { expect, test } from '@playwright/test'

test('river announces activations without blocking clicks', async ({ page }) => {
  await page.goto('/#/cards/01001a')
  await page.evaluate(async () => {
    const load = (path: string) => import(/* @vite-ignore */ path)
    const { createApp, h, ref } = await load('/node_modules/.vite/deps/vue.js')
    const { createI18n } = await load('/node_modules/.vite/deps/vue-i18n.js')
    const { default: River } = await load('/src/components/River.vue')
    const { loadLocaleMessages } = await load('/src/locales/messages.ts')
    const { messages } = await loadLocaleMessages('en')
    const alert = ref('Venom Goblin attacks Daredevil')
    const host = document.createElement('div')
    document.body.replaceChildren(host)
    createApp({ setup: () => () => h(River, { cards: [], elsewhere: new Set(), activationAlert: alert.value }) })
      .use(createI18n({ legacy: false, locale: 'en', messages: { en: messages } })).mount(host)
    Object.assign(window, { riverAlert: alert })
  })
  const alert = page.getByRole('status')
  await expect(alert).toHaveText('Venom Goblin attacks Daredevil')
  expect(await alert.evaluate(el => getComputedStyle(el).pointerEvents)).toBe('none')
  for (const text of ['Venom Goblin schemes']) {
    await page.evaluate(text => { (window as any).riverAlert.value = text }, text)
    await expect(alert).toHaveText(text)
  }
})

for (const status of ['stunned', 'confused'] as const) {
  test(`river explains an event canceled by ${status}`, async ({ page }) => {
    await page.goto('/#/cards/01001a')
    await page.evaluate(async (status) => {
      const load = (path: string) => import(/* @vite-ignore */ path)
      const { createApp, h } = await load('/node_modules/.vite/deps/vue.js')
      const { createI18n } = await load('/node_modules/.vite/deps/vue-i18n.js')
      const { default: River } = await load('/src/components/River.vue')
      const { loadLocaleMessages } = await load('/src/locales/messages.ts')
      const { messages } = await loadLocaleMessages('en')
      const host = document.createElement('div')
      document.body.replaceChildren(host)
      createApp({ setup: () => () => h(River, {
        cards: [], elsewhere: new Set(), shared: true,
        canceledEvent: { code: status === 'stunned' ? '01008' : '01060', statuses: [status] },
      }) })
        .use(createI18n({ legacy: false, locale: 'en', messages: { en: messages } }))
        .mount(host)
    }, status)
    const river = page.locator('.river')
    await expect(river).toContainText(status === 'stunned' ? 'Stunned: attack canceled' : 'Confused: thwart canceled')
    await expect(river.locator('.card')).toHaveCount(1)
    const image = river.locator('.canceled-status')
    await expect(image).toBeVisible()
    expect(await image.evaluate((img: HTMLImageElement) => img.complete && img.naturalWidth > 0)).toBe(true)
  })
}

for (const status of ['stunned', 'confused'] as const) {
  test(`river warns about ${status} while paying`, async ({ page }) => {
    await page.goto('/#/cards/01001a')
    await page.evaluate(async (status) => {
      const load = (path: string) => import(/* @vite-ignore */ path)
      const { createApp, h } = await load('/node_modules/.vite/deps/vue.js')
      const { createI18n } = await load('/node_modules/.vite/deps/vue-i18n.js')
      const { default: PayingTray } = await load('/src/components/PayingTray.vue')
      const { loadLocaleMessages } = await load('/src/locales/messages.ts')
      const { messages } = await loadLocaleMessages('en')
      const host = document.createElement('div')
      host.style.setProperty('--card-width', '100px')
      document.body.replaceChildren(host)
      createApp({ setup: () => () => h(PayingTray, {
        elsewhere: new Set(), cost: {
          player: 'test', card: { id: 'event', code: '01060', name: 'For Justice!' },
          source: null, paid: [], owed: [null, null], anyAmount: false, spent: [], canceledBy: [status],
        },
      }) })
        .use(createI18n({ legacy: false, locale: 'en', messages: { en: messages } }))
        .mount(host)
    }, status)
    await expect(page.getByRole('status')).toContainText(status === 'stunned' ? 'Stunned will cancel this attack' : 'Confused will cancel this thwart')
    await expect(page.locator('.canceling-status')).toBeVisible()
  })
}

for (const reaction of ['Responses', 'Interrupts']) {
  test(`${reaction} stay left and fit their content beside an attack, then fill an empty river`, async ({ page }) => {
    await page.goto('/#/cards/01001a')
    await page.evaluate(async (reaction) => {
      const load = (path: string) => import(/* @vite-ignore */ path)
      const { createApp, h, ref } = await load('/node_modules/.vite/deps/vue.js')
      const { createI18n } = await load('/node_modules/.vite/deps/vue-i18n.js')
      const { default: River } = await load('/src/components/River.vue')
      const { loadLocaleMessages } = await load('/src/locales/messages.ts')
      const { messages } = await loadLocaleMessages('en')
      const host = document.createElement('div')
      host.style.setProperty('--card-width', '100px')
      document.body.replaceChildren(host)
      const alone = ref(false)
      Object.assign(window, { showReactionAlone: () => { alone.value = true } })
      createApp({ setup: () => () => h(River, {
        cards: alone.value ? [] : [{ id: 'boost', code: '01186' }], elsewhere: new Set(),
        label: alone.value ? null : 'Rhino attacks Spider-Man',
        attacker: alone.value ? null : { id: 'villain', code: '01094', name: 'Rhino', attack: 3 },
        reactionLabel: reaction,
        reactionCards: [{ id: 'response', code: '01056', name: 'Avengers Mansion' }],
        actions: [{ index: 7, kind: 'label', label: '$label.applyAttackDamage count=i:3' }],
        onChoose: (index: number) => { host.dataset.chosen = String(index) },
      }) }).use(createI18n({ legacy: false, locale: 'en', messages: { en: messages } })).mount(host)
    }, reaction)
    const responses = page.getByRole('region', { name: reaction, exact: true })
    await expect(responses.locator('[data-card-id="response"]')).toBeVisible()
    await expect(responses.locator('[data-card-id="villain"], [data-card-id="boost"]')).toHaveCount(0)
    const attack = page.locator('.lane').filter({ hasText: 'Rhino attacks Spider-Man' })
    await expect(attack.locator('[data-card-id="villain"]')).toBeVisible()
    await expect(attack.locator('[data-card-id="boost"]')).toBeVisible()
    await expect(attack.locator('[data-card-id="response"]')).toHaveCount(0)
    await attack.locator('.river-actions button').click()
    await expect(page.locator('.lane')).toHaveCount(2)
    await expect(page.locator('.lane').first()).toHaveAttribute('aria-label', reaction)
    const responseBox = (await responses.boundingBox())!
    const attackBox = (await attack.boundingBox())!
    expect(responseBox.x).toBeLessThan(attackBox.x)
    expect(responseBox.width).toBeLessThan(attackBox.width)
    await expect(page.locator('[data-chosen="7"]')).toHaveCount(1)
    await page.evaluate(() => (window as any).showReactionAlone())
    await expect(page.locator('.lane')).toHaveCount(1)
    await expect(responses).not.toHaveClass(/compact/)
    expect((await responses.boundingBox())!.width).toBeCloseTo((await page.locator('.river').boundingBox())!.width, 0)
  })
}

for (const status of ['stunned', 'confused'] as const) {
  for (const reduced of [false, true]) {
    test(`${status} flood has a clean first and last frame${reduced ? ' with reduced motion' : ''}`, async ({ page }) => {
      await page.setViewportSize({ width: 390, height: 844 })
      await page.emulateMedia({ reducedMotion: reduced ? 'reduce' : 'no-preference' })
      await page.goto('/#/cards/01001a')
      await page.evaluate(async status => {
        const load = (path: string) => import(/* @vite-ignore */ path)
        const { createApp, h, ref } = await load('/node_modules/.vite/deps/vue.js')
        const { createI18n } = await load('/node_modules/.vite/deps/vue-i18n.js')
        const { default: River } = await load('/src/components/River.vue')
        const { loadLocaleMessages } = await load('/src/locales/messages.ts')
        const { messages } = await loadLocaleMessages('en')
        const alert = ref<string | null>(null)
        const host = document.createElement('div')
        document.body.replaceChildren(host)
        createApp({ setup: () => () => h(River, { cards: [], elsewhere: new Set(), activationAlert: alert.value, activationBanner: true, activationKind: status }) })
          .use(createI18n({ legacy: false, locale: 'en', messages: { en: messages } })).mount(host)
        Object.assign(window, { statusAlert: alert })
        alert.value = status === 'stunned' ? 'Stunned: attack canceled' : 'Confused: thwart canceled'
      }, status)
      const alert = page.locator('.activation-alert')
      await expect(alert).toBeAttached()
      const frames = await alert.evaluate((el, reduced) => {
        const animation = el.getAnimations().find(animation => (animation as CSSAnimation).animationName.startsWith('fight-flood'))
        const read = () => ({ opacity: getComputedStyle(el).opacity, clip: getComputedStyle(el).clipPath })
        const color = getComputedStyle(el).backgroundColor
        if (reduced) return { color, animation: !!animation, first: read(), middle: read(), last: read() }
        animation!.pause()
        animation!.currentTime = 0
        const first = read()
        animation!.currentTime = 900
        const middle = read()
        animation!.currentTime = 1800
        return { color, animation: !!animation, first, middle, last: read() }
      }, reduced)
      expect(frames.color).toBe(status === 'stunned' ? 'rgb(244, 210, 50)' : 'rgb(117, 68, 140)')
      if (reduced) {
        expect(frames.animation).toBe(false)
        expect(frames.first.opacity).toBe('1')
      } else {
        expect(frames.first.opacity).toBe('0')
        expect(frames.first.clip).toContain('100%')
        expect(frames.middle.opacity).toBe('1')
        expect(frames.last.opacity).toBe('0')
      }
      expect(await page.evaluate(async () => {
        const { transitionsOn } = await import(/* @vite-ignore */ '/src/composables/useViewTransition.ts')
        return transitionsOn()
      })).toBe(false)
      await page.evaluate(() => { (window as any).statusAlert.value = null })
      await expect(alert).toHaveCount(0)
    })
  }
}
