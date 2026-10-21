import { defineConfig } from 'vite'
import vue from '@vitejs/plugin-vue'
import { fileURLToPath } from 'node:url'

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [vue()],
  resolve: {
    // the card browser, ported from its own app, imports by this root
    alias: { '@browser': fileURLToPath(new URL('./src/browser', import.meta.url)) },
  },
  server: {
    proxy: {
      '^/api': {
        target: `http://localhost:${process.env.API_PORT ?? 3000}/`,
        changeOrigin: true,
        secure: false,
        ws: true
      }
    }
  },
})
