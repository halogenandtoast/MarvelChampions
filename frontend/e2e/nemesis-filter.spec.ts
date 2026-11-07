import { expect, test } from '@playwright/test'

test('nemesis sets can be selected and restored from the card browser URL', async ({ page }) => {
  await page.goto('/#/cards')
  await page.getByRole('button', { name: 'Nemesis sets', exact: true }).click()
  await page.getByRole('searchbox', { name: 'Search Nemesis sets', exact: true }).fill('Luke Cage')
  const group = page.locator('.group').filter({ has: page.getByRole('button', { name: 'Nemesis sets', exact: true }) })
  await group.getByRole('checkbox', { name: /Luke Cage/ }).check()
  await expect(page).toHaveURL(/set=luke_cage_nemesis/)
  await expect(page.getByRole('img', { name: 'Cottonmouth', exact: true })).toBeVisible()
  await page.reload()
  await expect(page).toHaveURL(/set=luke_cage_nemesis/)
  await expect(page.getByRole('img', { name: 'Cottonmouth', exact: true })).toBeVisible()
})
