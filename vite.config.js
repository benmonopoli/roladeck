import { defineConfig } from "vite";
import melange from "vite-plugin-melange";

export default defineConfig({
  plugins: [
    melange({
      buildCommand: "opam exec --switch=ahrefs-recruit -- dune build",
      watchCommand: "opam exec --switch=ahrefs-recruit -- dune build --watch",
      emitDir: "frontend",
      buildTarget: "frontend",
    }),
  ],
  server: {
    port: 3000,
    proxy: {
      "/api": {
        target: "http://localhost:4000",
        changeOrigin: true,
      },
    },
  },
  build: {
    outDir: "dist",
  },
});
