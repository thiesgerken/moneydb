'use strict';

import IBAN from 'iban';
import page from './securities_accounts_modal.html';
import $ from '/jquery.js';

export function init() {
  $('body').append(page);

  $.fn.form.settings.rules.iban = function(value) {
    return IBAN.isValid(value) || value == '';
  };

  $('#modal-securities-accounts .ui.form').form({
    fields: {
      'modal-securities-accounts-title': {
        rules: [{
          type: 'empty',
          prompt: 'Titel darf nicht leer sein'
        }]
      },
      'modal-securities-accounts-broker': {
        rules: [{
          type: 'empty',
          prompt: 'Broker darf nicht leer sein'
        }]
      },
      'modal-securities-accounts-iban': {
        rules: [{
          type: 'iban',
          prompt: 'IBAN ist ungültig'
        }]
      }
    }
  });
}

export function reset() {
  $('#modal-securities-accounts .ui.form').form('clear');
  $('#modal-securities-accounts .ui.error.message').empty();
}

export function load(a) {
  reset();

  $('#modal-securities-accounts-iban').val(a.iban);
  $('#modal-securities-accounts-title').val(a.title);
  $('#modal-securities-accounts-broker').val(a.broker);
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

  a.title = $('#modal-securities-accounts-title').val();
  a.iban = IBAN.printFormat($('#modal-securities-accounts-iban').val(), ' ');
  a.broker = $('#modal-securities-accounts-broker').val();

  return a;
}
