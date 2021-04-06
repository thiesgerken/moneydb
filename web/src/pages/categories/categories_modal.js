'use strict';

import tinycolor from 'tinycolor2';
import page from './categories_modal.html';
import $ from '/jquery.js';
import * as Util from '/util.js';
import * as Common from '/common.js';

let initReplacesDropdown = function(location) {
  let pr = $(location).parent();
  let prm = pr.find('.menu').empty();

  for (let i = 0; i < Common.visibleCategories.length; i++) {
    let a = Common.visibleCategories[i];

    if (a.ownerId != Common.me.id)
      prm.append('<div class="item" data-value="' + a.id + '">' + '<div class="color-box" style="background-color: ' + a.color + ';"></div>&nbsp;' + Common.categoryToString(a) + '</div>');
  }
};

export function init() {
  $('body').append(page);

  $('.ui.form', '#modal-categories').form({
    fields: {
      title: {
        rules: [{
          type: 'empty',
          prompt: 'Titel darf nicht leer sein'
        }]
      },
      color: {
        rules: [{
            type: 'empty',
            prompt: 'Farbe darf nicht leer sein'
          },
          {
            type: 'color',
            prompt: 'Farbwert wurde nicht erkannt'
          }
        ]
      }
    }
  });

  Util.initColorButtons($('#modal-categories'));

  $('#color', '#modal-categories').change(function() {
    let v = $('#color', '#modal-categories').val();
    let c = tinycolor(v);

    if (c.isValid()) {
      $('#color', '#modal-categories').val(c.toHexString());

      if (c.getBrightness() < 100)
        $('#color', '#modal-categories').attr('style', 'color: #F7F7F7; background-color: ' + c.toHexString());
      else
        $('#color', '#modal-categories').attr('style', 'background-color: ' + c.toHexString());
    } else
      $('#color', '#modal-categories').attr('style', '');
  });
}

export function reset() {
  initReplacesDropdown($('#replaces', '#modal-categories'));

  $('#color', '#modal-categories').attr('style', '');
  $('.ui.form', '#modal-categories').form('clear');
  $('.ui.error.message', '#modal-categories').empty();
}

export function load(c) {
  reset();

  $('#title', '#modal-categories').val(c.title);
  $('#description', '#modal-categories').val(c.description);
  $('#color', '#modal-categories').val(c.color).trigger('change');
  $('#replaces', '#modal-categories').parent().dropdown('set exactly', c.replaces.map(function(v) {
    return v.toString();
  })).dropdown('refresh');
}

export function save(w) {
  let c;

  if (w == undefined)
    c = {
      id: -1,
      ownerId: -1,
      replaces: []
    };
  else
    c = $.extend(true, {}, w); // avoid changing of w

  c.title = $('#title', '#modal-categories').val();
  c.description = $('#description', '#modal-categories').val();
  c.color = tinycolor($('#color', '#modal-categories').val()).toHexString();

  let repl = $('#replaces', '#modal-categories').parent().dropdown('get value');
  if (repl == '')
    c.replaces = [];
  else
    c.replaces = repl.split(',').map(function(v) {
      return parseInt(v);
    });

  return c;
}
