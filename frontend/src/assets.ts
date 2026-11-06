/** Empty in local development; the CDN origin in deployed builds. */
export const ASSET_HOST = (import.meta.env.VITE_ASSET_HOST ?? '').replace(/\/+$/, '')

export const assetUrl = (path: string) => `${ASSET_HOST}${path}`
