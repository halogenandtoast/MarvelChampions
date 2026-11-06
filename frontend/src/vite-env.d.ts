/// <reference types="vite/client" />

interface ImportMetaEnv {
  /** CDN origin for images and rulebooks; empty serves local public assets. */
  readonly VITE_ASSET_HOST?: string
}
