// ref: https://github.com/rust-lang/mdBook/issues/2467#issuecomment-2857375717

// shiki vs prismjs 
//   - shiki: https://shiki.style/languages
//   - prismjs: https://prismjs.com/#supported-languages

let shikiModule

window.hljs = {
    configure() {
        shikiModule = import('https://esm.sh/shiki@3.8.1')
    },
    /** @param {HTMLElement} block */
    async highlightBlock(block) {
        const lang = [...block.classList.values()]
            .map(name => name.match(/^language-(.+)$/)?.[1])
            .filter(Boolean)[0]
        if (!lang) {
            return
        }
        const shiki = await shikiModule
        block.parentElement.innerHTML =
            await shiki.codeToHtml(block.innerText, { lang, theme: 'one-light' })
    }
}