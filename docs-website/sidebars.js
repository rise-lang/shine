module.exports = {
  docs: [
    {
      type: 'category',
      label: 'Getting started',
      collapsed: false,
      items: [
        'greeting',
        'setup'
      ],
    },
    {
      type: 'category',
      label: 'Tutorials',
      collapsed: false,
      items: [
        'tutorial',
        'exploration/tutorial'
      ],
    },
    {
      type: 'category',
      label: 'Technical Documentation',
      collapsed: false,
      items: [
        'exploration/exploration'
        // {
        //   type: 'category',
        //   label: 'Exploration',
        //   collapsed: true,
        //   items: [
        //     'exploration/exploration'
        //   ],
        // },
      ],
    }
  ],
  docs: {
    'Getting started': ['greeting', 'setup'],
    'Tutorials': ['overview'],
    'Language Reference': ['reference/rise-primitives']
  },
};
