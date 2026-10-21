/// <reference types="vite/client" />

interface ImportMetaEnv {
  /** Where card images are served from, when not this site (an S3 bucket, say). */
  readonly VITE_ASSET_HOST?: string
}
