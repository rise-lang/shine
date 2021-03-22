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
        },
        theme: {
          customCss: require.resolve('./src/css/custom.css'),
        },
      },
    ],
  ],
};
