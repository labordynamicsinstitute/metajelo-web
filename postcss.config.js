module.exports = {
  plugins: [
    require('autoprefixer'),
    require('cssnano')({
      preset: ['default', {
        reduceTransforms: true
      }]
    })
  ]
};
