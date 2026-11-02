import { expect, test } from '@playwright/test'

// These cards exercise scanned text, legacy import links, and cards that change type.
test('Shadowland legacy links show reviewed text and the correct reverse face', async ({ page }) => {
  await page.goto('/#/cards/shadowland-1-004a')
  await expect(page.getByRole('heading', { name: 'The Streets Below', exact: true })).toBeVisible()
  await expect(page.locator('.block')).toContainText('Spend')
  await expect(page.locator('.block .physical')).toHaveCount(2)
  await expect(page.getByText(/unverified/i)).toHaveCount(0)
  await page.getByRole('tab', { name: 'Atop the Tenshu', exact: true }).click()
  await expect(page.locator('.block')).toContainText('Deal 3 damage to your identity.')
  await expect(page.locator('.block .energy')).toHaveCount(2)
})

test('Fear No Evil encounter cards display rules, printed stats and local art', async ({ page }) => {
  await page.goto('/#/cards/60065')
  await expect(page.getByRole('heading', { name: /Bullseye$/ })).toBeVisible()
  await expect(page.locator('.block')).toContainText('each of his boost cards gains')
  await expect(page.locator('.stat[data-stat="health"] .value')).toHaveText('14★')
  await expect(page.locator('.block .boost')).toHaveCount(1)
  const image = page.locator('.art img').first()
  await expect(image).toHaveAttribute('src', '/images/cards/60065.avif')
  await expect.poll(() => image.evaluate((el: HTMLImageElement) => el.naturalWidth)).toBeGreaterThan(0)
})

test('Establish Trust turns over into the Mary Walker attachment', async ({ page }) => {
  await page.goto('/#/cards/60113a')
  await expect(page.getByRole('heading', { name: 'Establish Trust', exact: true })).toBeVisible()
  await page.getByRole('tab', { name: 'Mary Walker', exact: true }).click()
  await expect(page.getByRole('heading', { name: 'Mary Walker', exact: true })).toBeVisible()
  await expect(page.locator('.body .line')).toContainText('Attachment')
  await expect(page.locator('.block')).toContainText('Attached villain cannot activate.')
})

test('both releases have complete text, valid faces and no third-party image links', async ({ request }) => {
  const response = await request.get('/data/cards.json')
  const index = await response.json()
  const cards = index.cards.filter((c: { packCode: string }) => ['fne', 'shadowland'].includes(c.packCode))
  expect(cards.filter((c: { packCode: string }) => c.packCode === 'shadowland')).toHaveLength(93)
  expect(cards.filter((c: { packCode: string }) => c.packCode === 'fne')).toHaveLength(239)
  const codes = new Set(index.cards.map((c: { code: string }) => c.code))
  for (const card of cards) {
    expect(card.text?.trim(), card.code).toBeTruthy()
    if (card.linkedTo) expect(codes.has(card.linkedTo), card.code).toBe(true)
    expect(JSON.stringify(card), card.code).not.toContain('marvelsimulator')
    if (card.textVerified) expect(card.bundledImage, card.code).toBe(true)
  }
})
