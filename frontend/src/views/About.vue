<template>
  <div class="page-container">
    <div class="about">
      <h1 class="page-title">{{ $t('nav.about') }}</h1>
      <section>
        <h2>{{ $t('about.copyrightTitle') }}</h2>
        <div class="box panel">
          <i18n-t keypath="about.copyrightBody" tag="p" scope="global">
            <template #game><a href="https://www.fantasyflightgames.com/en/products/marvel-champions-the-card-game/" rel="noreferrer" target="_blank">{{ $t('about.gameName') }}</a></template>
          </i18n-t>
          <p>{{ $t('about.copyrightMarvel') }}</p>
          <p>{{ $t('about.copyrightArtwork') }}</p>
        </div>
      </section>
      <section>
        <h2>{{ $t('about.dataTitle') }}</h2>
        <div class="box panel">
          <i18n-t keypath="about.dataBody" tag="p" scope="global">
            <template #marvelcdb><a href="https://marvelcdb.com" rel="noreferrer" target="_blank">MarvelCDB</a></template>
          </i18n-t>
        </div>
      </section>
      <section>
        <h2>{{ $t('about.projectTitle') }}</h2>
        <div class="box panel">
          <i18n-t keypath="about.projectBody" tag="p" scope="global">
            <template #author><a href="https://github.com/halogenandtoast" rel="noreferrer" target="_blank">halogenandtoast</a></template>
            <template #source><a href="https://github.com/halogenandtoast/MarvelChampions" rel="noreferrer" target="_blank">GitHub</a></template>
          </i18n-t>
        </div>
      </section>
    </div>
  </div>
</template>

<style scoped>
.about { width: min(720px, 100%); margin: 0 auto; display: grid; gap: 18px; }
.about .page-title { margin: 0; }
section { display: grid; gap: 8px; }
h2 { margin: 0; font-size: 1.3rem; text-transform: uppercase; color: var(--text-muted); }
.panel { line-height: 1.6; color: var(--text-muted); display: grid; gap: 10px; }
.panel p { margin: 0; }
</style>
