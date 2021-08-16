const math = require('remark-math');
const katex = require('rehype-katex');
module.exports = {
  title: 'RISE Language Documentation',
  tagline: 'A functional pattern-based data-parallel language',
  url: 'https://rise-lang.org',
  baseUrl: '/doc/',
  onBrokenLinks: 'throw',
  onBrokenMarkdownLinks: 'warn',
  favicon: 'img/favicon.ico',
  organizationName: 'rise-lang', // Usually your GitHub org/user name.
  projectName: 'doc', // Usually your repo name.
  stylesheets: [
    {
      href: 'https://cdn.jsdelivr.net/npm/katex@0.13.2/dist/katex.min.css',
      type: 'text/css',
      integrity:
          'sha384-Cqd8ihRLum0CCg8rz0hYKPoLZ3uw+gES2rXQXycqnL5pgVQIflxAUDS7ZSjITLb5',
      crossorigin: 'anonymous',
    },
  ],
  themeConfig: {
    navbar: {
      title: 'RISE',
      logo: {
        alt: 'RISE Logo',
        src: 'img/logoDark.svg',
      },
      style: 'dark',
      items: [
        {
          href: 'https://github.com/rise-lang/shine',
          label: 'GitHub',
          position: 'right',
        },
      ],
    },
    prism: {
      additionalLanguages: ['scala'],
      theme: require('prism-react-renderer/themes/dracula'),
    },
    footer: {
      style: 'dark',
      links: [
      ],
      copyright: `Copyright © ${new Date().getFullYear()} The RISE language team.`,
    },
  },
  presets: [
    [
      '@docusaurus/preset-classic',
      {
        docs: {
          path: 'docs',
          routeBasePath: '/',
          sidebarPath: require.resolve('./sidebars.js'),
          remarkPlugins: [math],
          rehypePlugins: [katex],
        },
        theme: {
          customCss: require.resolve('./src/css/custom.css'),
        },
      },
    ],
  ],
};
