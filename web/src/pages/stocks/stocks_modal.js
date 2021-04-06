'use strict';

import page from './stocks_modal.html';
import $ from '/jquery.js';

function luhn_check(value) {
  let vs = '';

  // convert characters to digits 
  value.split('').forEach((x) => {
    if (isNaN(parseInt(x)))
      vs += (x.charCodeAt(0) - 65 + 10).toString();
    else
      vs += x;
  });

  return vs.split('')
    .reverse()
    .map((x) => parseInt(x))
    .map((x, idx) => idx % 2 ? x * 2 : x)
    .map((x) => x > 9 ? (x % 10) + 1 : x)
    .reduce((accum, x) => accum += x) % 10 === 0;
}

export function init() {
  $('body').append(page);

  $.fn.form.settings.rules.isin = function(value) {
    return value.length == 12 && /[A-Z]{2}[A-Z0-9]{10}/.test(value);
  };

  $.fn.form.settings.rules.luhn = luhn_check;

  $('#modal-stocks .ui.form').form({
    fields: {
      'modal-stocks-isin': {
        rules: [{
            type: 'isin',
            prompt: 'ISIN hat falsche Form'
          },
          {
            type: 'luhn',
            prompt: 'ISIN hat Fehler in Prüfsumme'
          }
        ]
      }
    }
  });
}

export function reset() {
  $('#modal-stocks .ui.form').form('clear');
  $('#modal-stocks .ui.error.message').empty();
}

export function load(a) {
  reset();

  $('#modal-stocks-isin').val(a.isin);
}

export function save(w) {
  let a;

  if (w == undefined)
    a = {
      id: -1,
      ownerId: -1
    };
  else
    a = $.extend(true, {}, w); // avoid changing of w

  a.ownerId = -1;
  a.isin = $('#modal-stocks-isin').val();
  return a;
}
