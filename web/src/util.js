'use strict';
// @flow

import $ from '/jquery.js';
import tinycolor from 'tinycolor2';
import Please from 'pleasejs';

export let datatableLanguage = {
  'decimal': ',',
  'emptyTable': 'Keine Daten verf&uuml;gbar',
  'info': 'Eintr&auml;ge _START_ bis _END_ von _TOTAL_ Eintr&auml;gen',
  'infoEmpty': 'Eintr&auml;ge 0 bis 0 von 0 Eintr&auml;gen',
  'infoFiltered': '(von gesamt _MAX_ Eintr&auml;gen)',
  'infoPostFix': '',
  'thousands': '',
  'lengthMenu': 'Zeige _MENU_ Eintr&auml;ge',
  'loadingRecords': 'Loading...',
  'processing': 'Loading...',
  'search': '', // Suche:
  'zeroRecords': 'Keine passenden Eintr&auml;ge verf&uuml;gbar',
  'paginate': {
    'first': 'Erste',
    'last': 'Letzte',
    'next': 'N&auml;chste',
    'previous': 'Vorherige'
  },
  'aria': {
    'sortAscending': ': aufsteigend sortieren',
    'sortDescending': ': absteigend sortieren'
  }
};

export function isInt(val) {
  let intRegex = /^-?\d+$/;
  if (!intRegex.test(val))
    return false;

  let intVal = parseInt(val, 10);
  return parseFloat(val) == intVal && !isNaN(intVal);
}

export function isFloat(value) {
  let floatRegex = /^-?\d+(?:[.,]\d*?)?$/;
  if (!floatRegex.test(value))
    return false;

  let val = parseFloat(value);
  if (isNaN(val))
    return false;

  return true;
}

export function unknownUser(id) {
  return {
    id: id,
    fullName: '[Unknown User]',
    name: '[unknown]'
  };
}

export function unknownAccount(id) {
  return {
    id: id,
    title: '[unknown]',
    ownerId: -1
  };
}

export function unknownCategory(id) {
  return {
    id: id,
    title: '[unknown]',
    ownerId: -1
  };
}

export function setBackgroundColor(object, color, darker) {
  color = tinycolor(color);

  if (darker) {
    color = color.desaturate(10).darken(5);
  }

  if (color.getBrightness() < 100) {
    $(object).css('color', '#F7F7F7');
  } else {
    $(object).attr('style', '');
  }

  $(object).css('background-color', color.toString());
}

export function flattenRecord(t) {
  if (t === null) return null;

  let o = t.data;
  o.id = t.id;

  return o;
}

export function nullToNaN(x) {
  return x == null ? NaN : x;
}

export function textEllipsis(s, w) {
  return s.length > w ? (s.substring(0, w - 4) + ' ...') : s;
}

export function formatAdditions(i) {
  if (i == 0)
    return '';
  else if (i == 1)
    return ' + eine Ausgabe';
  else
    return ' + ' + i.toString() + ' Ausgaben';
}

export function formatAmount(x, symbol) {
  if (typeof symbol === 'undefined')
    symbol = '&euro;';

  if (typeof x === 'undefined' || x == null || isNaN(x))
    return 'NaN ' + symbol;

  return x.toFixed(2).replace('.', ',') + ' ' + symbol;
}

export function formatPercentage(x) {
  if (typeof x === 'undefined' || x == null || isNaN(x))
    return 'NaN %';

  return (x * 100).toFixed(2).replace('.', ',') + ' %';
}

export function availabilityToString(av) {
  switch (av) {
    case 'Immediately':
      return 'Sofort';
    case 'Weeks':
      return '&le; Wochen';
    case 'Months':
      return '&le; Monate';
    case 'Years':
      return '&le; Jahre';
    case 'Decades':
      return '&le; Jahrzehnte';
  }

  return '??';
}

export function errorMsg(s, e) {
  // console.log(s);
  // if (typeof e !== 'undefined') {
  //   console.log(e);
  // }

  if (typeof e != 'undefined')
    $('#modal-error .content').html('<p style="font-size: 120%; text-align:center">' + s + '</p><p style="font-size: 110%; text-align:center">Antwort des Servers war ' + e.status + ' - ' + e.statusText + ': ' + e.responseText + '</p>');
  else
    $('#modal-error .content').html('<p style="font-size: 120%; text-align:center">' + s + '</p>');

  $('#modal-error').modal('show');
}

export function initColorButtons(location) {
  $('.color-random', location).click(function() {
    let input = $(this).parent().find('input');

    input.val(Please.make_color({
      saturation: 0.8
    }));

    input.change();
  });

  $('.color-saturation-minus', location).click(function() {
    let input = $(this).parent().find('input');

    input.val(tinycolor(input.val()).desaturate(10));
    input.change();
  });

  $('.color-saturation-plus', location).click(function() {
    let input = $(this).parent().find('input');

    input.val(tinycolor(input.val()).saturate(10));
    input.change();
  });
}
