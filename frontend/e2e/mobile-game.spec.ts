import { expect, test } from '@playwright/test'

test.use({ hasTouch: true })

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

for (const viewport of [{ width: 390, height: 844 }, { width: 320, height: 568 }, { width: 740, height: 390 }]) {
  test(`mobile decisions and cards remain reachable at ${viewport.width}x${viewport.height}`, async ({ page }) => {
    await page.setViewportSize(viewport)
    await page.addInitScript(() => localStorage.setItem('marvel-session', JSON.stringify({ token: 'test', user: { id: 1, username: 'Tester' } })))
    await page.route('**/api/games/mobile', route => route.fulfill({ json: snapshot }))
    await page.route('**/api/scenarios', route => route.fulfill({ json: { scenarios: [], modulars: [] } }))
    await page.route('**/api/whoami', route => route.fulfill({ json: { id: 1, username: 'Tester' } }))
    await page.routeWebSocket('**/api/games/mobile?*', socket => { socket.send(JSON.stringify({ tag: 'Update', contents: snapshot })) })
    await page.goto('/#/games/mobile')
    await expect(page.locator('#nav')).toBeHidden()
    const bar = page.locator('.game-bar')
    expect((await bar.boundingBox())!.height).toBe(44)
    await bar.getByRole('button', { name: 'Main menu', exact: true }).tap()
    const siteMenu = bar.locator('.site-menu .menu-items')
    await expect(siteMenu.getByRole('link', { name: 'Cards', exact: true })).toBeVisible()
    await expect(siteMenu.getByRole('link', { name: 'Settings', exact: true })).toBeVisible()
    await expect(siteMenu.getByRole('button', { name: 'Sign out', exact: true })).toBeVisible()
    await page.keyboard.press('Escape')
    await expect(siteMenu).toHaveCount(0)
    await bar.getByRole('button', { name: 'Game', exact: true }).tap()
    await expect(bar.getByRole('button', { name: 'File bug', exact: true })).toBeVisible()
    await page.keyboard.press('Escape')
    const dock = page.locator('.mobile-dock')
    await expect(dock).toBeVisible()
    await expect(dock.locator('.options button')).toHaveCount(3)
    const button = dock.locator('.options button').first()
    await expect(button).toContainText('Hand card 1')
    expect((await button.boundingBox())!.height).toBeGreaterThanOrEqual(48)
    expect((await button.boundingBox())!.height).toBeLessThanOrEqual(60)
    await expect(page.locator('.mobile-inspect')).toHaveCount(0)
    await page.screenshot({ path: test.info().outputPath('mobile.png') })
    expect((await dock.boundingBox())!.y + (await dock.boundingBox())!.height).toBeLessThanOrEqual(viewport.height + 1)
    expect(await page.evaluate(() => document.documentElement.scrollWidth)).toBeLessThanOrEqual(viewport.width)
    await expect(page.locator('.player-tableau')).toHaveCSS('zoom', '1')
    await page.getByRole('button', { name: /Actions/ }).click()
    await expect(page.locator('#mobile-decisions')).toBeHidden()
    await page.getByRole('button', { name: /Hand 7/ }).click()
    await expect(page.locator('.floating-hand')).toBeInViewport()
    const card = page.locator('.floating-hand .card').first()
    expect((await card.boundingBox())!.width).toBeCloseTo(108, 0)
    await page.getByRole('slider', { name: 'Card size' }).fill('160')
    await expect.poll(async () => (await card.boundingBox())!.width).toBeCloseTo(160, 0)
    const fan = page.locator('.fan')
    expect(await fan.evaluate(el => el.scrollWidth > el.clientWidth)).toBe(true)
    const requests: string[] = []
    page.on('request', request => { if (request.method() === 'POST') requests.push(request.url()) })
    await card.tap()
    await expect(page.locator('.card-overlay')).toBeVisible()
    expect(requests).toHaveLength(0)
    await page.touchscreen.tap(5, 5)
    await expect(page.locator('.card-overlay')).toHaveCount(0)
    await page.getByRole('button', { name: /Actions/ }).tap()
    const submitted = page.waitForRequest(request => request.url().endsWith('/api/games/mobile') && request.method() === 'POST')
    await dock.locator('.options button').first().tap()
    expect((await submitted).postDataJSON()).toMatchObject({ choice: 0 })
    await page.reload()
    await expect(page.getByRole('slider', { name: 'Card size' })).toHaveValue('160')
    await page.setViewportSize({ width: 1600, height: 1000 })
    await expect(dock).toHaveCount(0)
    await expect(page.locator('#nav')).toBeVisible()
    await expect(bar.locator('.standalone-bug')).toBeVisible()
    await expect(bar.locator('.mobile-site-nav')).toBeHidden()
    await expect(page.locator('.floating-hand')).toHaveCSS('position', 'absolute')
    await expect(card).not.toHaveAttribute('role', 'button')
  })
}
