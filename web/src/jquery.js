'use strict';

import tinycolor from 'tinycolor2';

export let $ = require('jquery');
export default $;

window.$ = $;
window.jQuery = $;

require('datatables.net-se')(window);
require('jquery-address');
// require('semantic-ui-calendar/dist/calendar.js');
require('/../semantic-ui/dist/semantic.js');

let isFloat = function(value) {
  let floatRegex = /^-?\d+(?:[.,]\d*?)?$/;
  if (!floatRegex.test(value))
    return false;

  let val = parseFloat(value);
  if (isNaN(val))
    return false;

  return true;
};

// initialize form rules
$.fn.form.settings.rules.isamount = v => isFloat(v.replace(',', '.'));
$.fn.form.settings.rules.isamountorempty = v => isFloat(v.replace(',', '.')) || v == '';
$.fn.form.settings.rules.nonzero = v => parseFloat(v.replace(',', '.')) != 0;
$.fn.form.settings.rules.color = v => tinycolor(v).isValid();
