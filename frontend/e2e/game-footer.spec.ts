import { expect, test } from '@playwright/test'

const hand = Array.from({ length: 7 }, (_, n) => ({ id: `hand-${n}`, code: '01077', name: `Hand card ${n + 1}` }))
const snapshot = {
  started: true, canUndo: true, playerId: 'hero', log: [],
  table: { owner: 1, withFriends: false, seats: 1, seated: [] },
  setup: { scenarioCode: 'rhino', modulars: [], difficulty: 'Normal' },
  game: {
    round: 2, phase: 'PlayerPhase', activePlayer: 'hero', playerOrder: ['hero'], seats: ['hero'], questionVersion: 3,
    mission: null, outcome: null, accelerationTokens: 0, focusedCards: [], encounterDiscard: [], encounterDeckSize: 20,
    players: [{ id: 'hero', code: '01001a', name: 'Spider-Man', faces: ['01001a', '01001b'], form: 'hero', hitPoints: 10, damage: 2,
      thwart: 1, attack: 2, defense: 3, handSize: 5, handCount: 7, hand, deckSize: 20, discard: [], allyLimit: 3, allies: [],
      supports: Array.from({ length: 9 }, (_, n) => ({ id: `support-${n}`, code: '01087', name: `Support ${n + 1}` })), upgrades: [],
    }],
    villains: [{ id: 'rhino', code: '01094', name: 'Rhino', hitPoints: 14, damage: 3, attack: 2, scheme: 1, attachments: [], upgrades: [] }],
    mainSchemes: [{ id: 'scheme', code: '01097', name: 'The Break-In!', threat: 2, threshold: 7 }], sideSchemes: [], minions: [],
    question: { hero: { kind: 'chooseOne', choices: [
      { index: 0, kind: 'play', target: 'hand-0' },
      { index: 1, kind: 'ability', target: 'hero', abilityType: 'Basic', label: '$label.attack' },
      { index: 2, kind: 'endTurn' },
    ] } },
  },
}

test('crowded encounters share a fitted card size and grow again when space returns', async ({ page }) => {
  const crowded = structuredClone(snapshot)
  crowded.game.players[0]!.supports = crowded.game.players[0]!.supports.slice(0, 3)
  crowded.game.villains = Array.from({ length: 4 }, (_, index) => ({ ...snapshot.game.villains[0]!, id: `villain-${index}` }))
  crowded.game.mainSchemes = Array.from({ length: 3 }, (_, index) => ({ ...snapshot.game.mainSchemes[0]!, id: `scheme-${index}` }))
  await page.addInitScript(() => localStorage.setItem('marvel-session', JSON.stringify({ token: 'test', user: { id: 1, username: 'Tester' } })))
  await page.route('**/api/games/fitting', route => route.fulfill({ json: crowded }))
  await page.route('**/api/scenarios', route => route.fulfill({ json: { scenarios: [], modulars: [] } }))
  await page.route('**/api/whoami', route => route.fulfill({ json: { id: 1, username: 'Tester' } }))
  await page.routeWebSocket('**/api/games/fitting?*', () => {})
  await page.setViewportSize({ width: 1440, height: 900 })
  await page.goto('/#/games/fitting')
  await expect(page.locator('.player-area')).toBeVisible()
  await page.evaluate(async () => {
    await document.fonts.ready
    await new Promise(requestAnimationFrame)
    await new Promise(requestAnimationFrame)
  })
  const measure = () => page.evaluate(() => {
    const board = document.querySelector<HTMLElement>('.board')!
    const content = document.querySelector<HTMLElement>('.player-half > .half')!
    return {
      width: parseFloat(board.style.getPropertyValue('--card-width')),
      overflow: content.scrollHeight - content.clientHeight,
      boardOverflow: board.scrollHeight - board.clientHeight,
      cardWidths: [...document.querySelectorAll<HTMLElement>('.villain .card, .in-hand .card')].map(card => card.offsetWidth),
    }
  })
  await expect.poll(async () => (await measure()).overflow).toBeLessThanOrEqual(1)
  const small = await measure()
  expect(small.width).toBeLessThan(124)
  expect(small.boardOverflow).toBeLessThanOrEqual(1)
  expect(Math.max(...small.cardWidths) - Math.min(...small.cardWidths)).toBeLessThanOrEqual(1)
  await page.setViewportSize({ width: 2200, height: 1500 })
  await expect.poll(async () => (await measure()).width).toBe(124)
  await page.setViewportSize({ width: 1440, height: 900 })
  await expect.poll(async () => (await measure()).width).toBe(small.width)
  await expect.poll(async () => (await measure()).overflow).toBeLessThanOrEqual(1)
  const widths = await page.evaluate(async () => {
    const values = []
    for (let n = 0; n < 30; n++) {
      await new Promise(requestAnimationFrame)
      values.push(document.querySelector<HTMLElement>('.board')!.style.getPropertyValue('--card-width'))
    }
    return [...new Set(values)]
  })
  expect(widths).toHaveLength(1)
  await page.setViewportSize({ width: 390, height: 844 })
  await expect.poll(() => page.locator('.board').evaluate(el => (el as HTMLElement).style.getPropertyValue('--card-width'))).toBe('')
})

test('refresh fits cards before paint and clips hand overhang without a scrollbar or extra gap', async ({ page }) => {
  await page.addInitScript(() => {
    localStorage.setItem('marvel-session', JSON.stringify({ token: 'test', user: { id: 1, username: 'Tester' } }))
    const start = document.startViewTransition.bind(document)
    ;(window as any).initialTransitions = 0
    document.startViewTransition = ((...args: Parameters<typeof start>) => {
      ;(window as any).initialTransitions++
      return start(...args)
    }) as typeof start
  })
  await page.route('**/api/games/footer', route => route.fulfill({ json: snapshot }))
  await page.route('**/api/scenarios', route => route.fulfill({ json: { scenarios: [], modulars: [] } }))
  await page.route('**/api/whoami', route => route.fulfill({ json: { id: 1, username: 'Tester' } }))
  await page.routeWebSocket('**/api/games/footer?*', () => {})
  await page.goto('/#/games/footer')
  for (const height of [1000, 768]) {
    await page.setViewportSize({ width: 1600, height })
    await page.reload()
    await expect(page.locator('.in-hand')).toHaveCount(7)
    expect(await page.evaluate(() => (window as any).initialTransitions)).toBe(0)
    const samples = await page.evaluate(async () => {
      const samples = []
      for (let frame = 0; frame < 30; frame++) {
        await new Promise(requestAnimationFrame)
        const area = document.querySelector('.player-area')! as HTMLElement
        const tableau = document.querySelector('.player-tableau')!.getBoundingClientRect()
        const style = getComputedStyle(area)
        const hand = document.querySelector('.hand')!.getBoundingClientRect()
        const cards = [...document.querySelectorAll('.in-hand .card')].map(card => card.getBoundingClientRect())
        samples.push({
          overflow: tableau.width - (area.clientWidth - parseFloat(style.paddingLeft) - parseFloat(style.paddingRight)),
          overhang: Math.max(...cards.map(card => card.bottom)) - hand.bottom,
        })
      }
      return samples
    })
    expect(Math.max(...samples.map(sample => sample.overflow))).toBeLessThanOrEqual(1)
    expect(Math.min(...samples.map(sample => sample.overhang))).toBeGreaterThan(0)
    const bounds = await page.evaluate(() => {
      const board = document.querySelector('.board')! as HTMLElement
      const zoneElement = document.querySelector('.player-half')!
      const zone = zoneElement.getBoundingClientRect()
      const footer = document.querySelector('#app > footer')!.getBoundingClientRect()
      const bottom = Math.max(...[...document.querySelectorAll('.in-hand .card')].map(card => card.getBoundingClientRect().bottom))
      board.scrollTop = board.scrollHeight
      return { scroll: board.scrollHeight - board.clientHeight, overhang: bottom - zone.bottom, clipping: getComputedStyle(zoneElement).overflowY, footerGap: footer.top - zone.bottom }
    })
    expect(bounds.scroll).toBeLessThanOrEqual(1)
    expect(bounds.overhang).toBeGreaterThan(0)
    expect(bounds.clipping).toBe('clip')
    expect(bounds.footerGap).toBeLessThanOrEqual(3)

  }
})

test('updating preserves facedown shading and clips card snapshots at the player boundary', async ({ page }) => {
  await page.addInitScript(() => localStorage.setItem('marvel-session', JSON.stringify({ token: 'test', user: { id: 1, username: 'Tester' } })))
  const data = structuredClone(snapshot)
  Object.assign(data.game.players[0], { upgrades: [{ id: 'inactive-form', code: '01087', name: 'Inactive form', facedown: true }] })
  await page.route('**/api/games/footer', route => route.fulfill({ json: data }))
  await page.route('**/api/scenarios', route => route.fulfill({ json: { scenarios: [], modulars: [] } }))
  await page.route('**/api/whoami', route => route.fulfill({ json: { id: 1, username: 'Tester' } }))
  await page.routeWebSocket('**/api/games/footer?*', () => {})
  await page.goto('/#/games/footer')
  const card = page.locator('[data-card-id="inactive-form"]')
  await expect(card).toBeVisible()
  await page.waitForFunction(() => [...document.querySelectorAll('img')].every(image => image.complete))
  await page.evaluate(() => document.fonts.ready)
  const bounds = await page.evaluate(() => {
    const area = document.querySelector('.player-half')!.getBoundingClientRect()
    const card = document.querySelector('[data-card-id="inactive-form"] .card-face')!.getBoundingClientRect()
    return {
      shade: { x: Math.floor(card.x + card.width / 2 - 10), y: Math.floor(card.y + card.height / 2 - 10), width: 20, height: 20 },
      below: { x: Math.ceil(area.x), y: Math.ceil(area.bottom) + 1, width: Math.floor(area.width) - 2, height: 8 },
    }
  })
  const shadeBefore = await page.screenshot({ clip: bounds.shade })
  const belowBefore = await page.screenshot({ clip: bounds.below })
  await page.evaluate(async () => {
    const transition = document.startViewTransition(() => { document.querySelector<HTMLElement>('.player-area')!.dataset.update = '1' })
    await transition.ready
    for (const animation of document.getAnimations()) {
      animation.pause()
      animation.currentTime = 140
    }
  })
  expect(await card.locator('.card-face').evaluate(element => getComputedStyle(element).filter)).toContain('brightness(0.55)')
  expect(await page.evaluate(() => getComputedStyle(document.documentElement, '::view-transition-group-children(player-half)').overflow)).toBe('clip')
  const shadeDuring = await page.screenshot({ clip: bounds.shade })
  const belowDuring = await page.screenshot({ clip: bounds.below })
  const difference = async (before: Buffer, after: Buffer) => page.evaluate(async ([a, b]) => {
    const pixels = async (data: string) => {
      const image = new Image()
      image.src = `data:image/png;base64,${data}`
      await image.decode()
      const canvas = document.createElement('canvas')
      canvas.width = image.width; canvas.height = image.height
      const context = canvas.getContext('2d')!
      context.drawImage(image, 0, 0)
      return context.getImageData(0, 0, image.width, image.height).data
    }
    const [left, right] = await Promise.all([pixels(a), pixels(b)])
    return left.reduce((sum, value, n) => sum + Math.abs(value - right[n]!), 0) / left.length
  }, [before.toString('base64'), after.toString('base64')])
  expect(await difference(shadeBefore, shadeDuring)).toBeLessThan(2)
  expect(await difference(belowBefore, belowDuring)).toBeLessThan(0.5)
  await page.evaluate(() => { for (const animation of document.getAnimations()) animation.finish() })
})
