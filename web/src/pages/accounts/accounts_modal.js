'use strict';

import tinycolor from 'tinycolor2';
import IBAN from 'iban';
import page from './accounts_modal.html';
import $ from '/jquery.js';
import * as Util from '/util.js';
import * as Common from '/common.js';

let initAccountSyncDropdown = function(location) {
  let pr = $(location).parent();
  let prm = pr.find('.menu').empty();

  for (let i = 0; i < Common.visibleAccounts.length; i++) {
    let a = Common.visibleAccounts[i];

    if (a.ownerId != Common.me.id)
      prm.append('<div class="item" data-value="' + a.id + '">' + '<div class="color-box" style="background-color: ' + a.color + ';"></div>&nbsp;' + Common.accountToString(a) + '</div>');
  }
};

export function init() {
  $('body').append(page);

  // $.fn.form.settings.rules.accsndaccount = function(value) {
  //   let t = $('#modal-accounts-type').parent().dropdown('get value');

  //   return t == 'Normal' || value != '';
  // };

  // $.fn.form.settings.rules.accsndfactor = function(value) {
  //   let t = $('#modal-accounts-type').parent().dropdown('get value');

  //   return t == 'Normal' || Util.isFloat(value.replace('.', ','));
  // };

  $.fn.form.settings.rules.iban = function(value) {
    return IBAN.isValid(value) || value == '';
  };

  $('#modal-accounts .ui.form').form({
    fields: {
      'modal-accounts-title': {
        rules: [{
          type: 'empty',
          prompt: 'Titel darf nicht leer sein'
        }]
      },
      'modal-accounts-iban': {
        rules: [{
          type: 'iban',
          prompt: 'IBAN ist ungültig'
        }]
      },
      // modal-accounts-factor: {
      //   rules: [{
      //     type: 'accsndfactor',
      //     prompt: 'Ungültiger Faktor'
      //   }]
      // },
      // modal-accounts-type: {
      //   rules: [{
      //     type: 'empty',
      //     prompt: 'Kein Typ angegeben'
      //   }]
      // },
      'modal-accounts-kind': {
        rules: [{
          type: 'empty',
          prompt: 'Keine Art angegeben'
        }]
      },
      'modal-accounts-availability': {
        rules: [{
          type: 'empty',
          prompt: 'Keine Verfügbarkeit angegeben'
        }]
      },
      // modal-accounts-sndaccount: {
      //   rules: [{
      //     type: 'accsndaccount',
      //     prompt: 'Kein zweites Konto angegeben'
      //   }]
      // },
      'modal-accounts-color': {
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

  Util.initColorButtons($('#modal-accounts'));

  $('#modal-accounts-color').change(function() {
    let v = $('#modal-accounts-color').val();
    let c = tinycolor(v);

    if (c.isValid()) {
      $('#modal-accounts-color').val(c.toHexString());

      if (c.getBrightness() < 100)
        $('#modal-accounts-color').attr('style', 'color: #F7F7F7; background-color: ' + c.toHexString());
      else
        $('#modal-accounts-color').attr('style', 'background-color: ' + c.toHexString());
    } else
      $('#modal-accounts-color').attr('style', '');
  });

  $('#modal-accounts-type').change(function() {
    let v = $(this).val();

    if (v == 'Normal') {
      $('#modal-accounts-sndaccount').parent().parent().hide();
      $('#modal-accounts-factor').parent().parent().hide();

      $('#modal-accounts-type').parent().parent().removeClass('six').removeClass('sixteen').removeClass('wide').addClass('sixteen').addClass('wide');
    } else {
      $('#modal-accounts-factor').parent().parent().show();
      $('#modal-accounts-sndaccount').parent().parent().show();
      $('#modal-accounts-type').parent().parent().removeClass('six').removeClass('sixteen').removeClass('wide').addClass('six').addClass('wide');
    }
  });
}

export function reset() {
  initAccountSyncDropdown($('#modal-accounts-sndaccount'));

  $('#modal-accounts-color').attr('style', '');
  $('#modal-accounts .ui.form').form('clear');
  $('#modal-accounts-type').val(0).trigger('change').parent().dropdown('refresh');
  $('#modal-accounts .ui.error.message').empty();
}

export function load(a) {
  reset();

  if (!a.syncing) {
    $('#modal-accounts-type').parent().dropdown('set selected', 'Normal');
  } else {
    $('#modal-accounts-type').parent().dropdown('set selected', 'Shared');
    let aid = a.id == a.syncing.account1 ? a.syncing.account2 : a.syncing.account1;

    $('#modal-accounts-sndaccount').parent().dropdown('set selected', aid.toString()).dropdown('refresh');
    $('#modal-accounts-factor').val(a.syncing.sign ? 'Gemeinsam' : 'Gegenteilig');
  }

  $('#modal-accounts-type').parent().dropdown('refresh');
  $('#modal-accounts-type').trigger('change');
  $('#modal-accounts-kind').parent().dropdown('set selected', a['kind']).dropdown('refresh');
  $('#modal-accounts-availability').parent().dropdown('set selected', a['availability']).dropdown('refresh');
  $('#modal-accounts-color').val(a.color).trigger('change');
  $('#modal-accounts-title').val(a.title);
  $('#modal-accounts-iban').val(a.iban);
  $('#modal-accounts-description').val(a.description);
  $('#modal-accounts-hidden').prop('checked', a.hidden);
}

export function save(w) {
  let a;

  if (w == undefined)
    a = {
      id: -1,
      ownerId: -1,
      syncing: undefined
    };
  else
    a = $.extend(true, {}, w); // avoid changing of w

  // var tt = $('#modal-accounts-type').parent().dropdown('get value');
  //
  // if (tt == 'Normal')
  //   a.syncing = {
  //     syncingType: "Normal",
  //     otherAccount: 0,
  //     factor: 0.0
  //   };
  // else
  //   a.syncing = {
  //     syncingType: "Synced",
  //     otherAccount: parseInt($('#modal-accounts-sndaccount').parent().dropdown('get value')),
  //     factor: parseFloat($('#modal-accounts-factor').val().replace(",", "."))
  //   };

  a.iban = $('#modal-accounts-iban').val();
  if (a.iban == '')
    a.iban = null;
  else
    a.iban = IBAN.printFormat(a.iban, ' ');

  a.title = $('#modal-accounts-title').val();
  a.description = $('#modal-accounts-description').val();
  a.color = $('#modal-accounts-color').val();
  a.hidden = $('#modal-accounts-hidden').prop('checked');
  a.kind = $('#modal-accounts-kind').parent().dropdown('get value');
  a.availability = $('#modal-accounts-availability').parent().dropdown('get value');

  return a;
}
