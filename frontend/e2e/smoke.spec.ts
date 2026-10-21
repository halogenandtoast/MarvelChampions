import { expect, test, type Page } from '@playwright/test'

// Runs against the dev stack: the game server on API_PORT, which this does not start.

const watchErrors = (page: Page) => {
  const errors: string[] = []
  page.on('pageerror', (e) => errors.push(e.message))
  page.on('console', (m) => m.type() === 'error' && errors.push(m.text()))
  return errors
}

test('the card browser finds a card and opens it', async ({ page }) => {
  const errors = watchErrors(page)
  await page.goto('/#/cards')
  await page.getByRole('searchbox', { name: 'Search cards' }).fill('Enhanced Spider')
  // the search waits for typing to pause
  const first = page.locator('.grid__button, .list__button').first()
  await expect(first).toContainText('Enhanced Spider-Sense')
  await first.click()
  await expect(page.locator('dialog .name')).toContainText('Enhanced Spider-Sense')
  expect(errors).toEqual([])
})

test('a new player deals a solo game against Rhino and answers its first question', async ({ page }) => {
  const errors = watchErrors(page)
  const name = `e2e${Date.now()}`
  await page.goto('/#/sign-up')
  await page.getByPlaceholder('Username').fill(name)
  await page.getByPlaceholder('Email').fill(`${name}@example.com`)
  await page.getByPlaceholder('Password').fill('password123')
  await page.getByRole('button', { name: 'Sign up' }).click()
  await page.getByRole('link', { name: 'New game' }).first().click()
  await page.locator('.villain', { hasText: 'Rhino' }).click()
  await page.getByRole('button', { name: 'Create' }).click()
  await expect(page.locator('.villain-area')).toBeVisible()
  await expect(page.locator('.player-area')).toBeVisible()
  const choice = page.locator('.question .options button, .card.active .chip').first()
  await expect(choice).toBeVisible()
  await choice.click()
  await expect(page.locator('.side .error')).toHaveCount(0)
  expect(errors).toEqual([])
})
