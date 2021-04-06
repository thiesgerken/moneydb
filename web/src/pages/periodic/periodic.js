'use strict';

import Table from '/table.js';
import tinycolor from 'tinycolor2';
import moment from 'moment/src/moment.js';
import $ from '/jquery.js';
import * as Util from '/util.js';
import * as Api from '/moneydb.js';
import * as PeriodicModal from './periodic_modal.js';

export let table;
export let location;
export let dataTable;

export function init(loc) {
  location = $(loc);

  let opts = {
    url: 'rules/periodic',
    location: $(location),
    defaultOrder: [
      [0, 'desc']
    ],
    lengths: [25, 50],
    expandable: false,
    formatter: function() {
      return '';
    },
    isReadOnly: () => false,
    isDeletable: () => true,
    colorRow: function(row) {
      $(row).find('td:nth-child(6)').attr('style', '');

      // setTdColor(rowjq.find("td:nth-child(3)"), tinycolor(data.color), false);
    },
    colorRowDark: function(row, data) {
      if (data.amountReplacement && data.amountReplacement != 0)
        $(row).find('td:nth-child(6)').attr('style', 'background-color:' + tinycolor(data.amountReplacement <= 0 ? '#FCFFF5' : '#FFF6F6').desaturate(10).darken(5).toString() + '!important');

      // setTdColor(rowjq.find("td:nth-child(3)"), tinycolor(data.color), true);
    },
    createdRow: function(row, data) {
      if (data.amountReplacement && data.amountReplacement != 0)
        $(row).find('td:nth-child(6)').addClass(data.amountReplacement <= 0 ? 'positive' : 'negative');
    },
    columns: [{
      data: 'id',
      title: 'ID',
      width: '2.5em'
    }, {
      data: 'title',
      title: 'Name der Regel',
      width: '16em'
    }, {
      data: 'templateId',
      title: 'ID der Vorlage',
      width: '8em'
    }, {
      data: 'period',
      title: 'Intervall',
      width: '8em',
      render: function(data, type, row) {
        switch (row.period) {
          case 'Yearly':
            return 'Jährlich';
          case 'Monthly':
            return 'Monatlich';
          case 'Weekly':
            return 'Wöchentlich';
          case 'Daily':
            return 'Täglich';
          default:
            return row.period;
        }
      }
    }, {
      data: 'titleReplacement',
      title: 'Ersatz für Ausgabentitel',
      width: '12em'
    }, {
      data: 'amountReplacement',
      title: 'Ersatz für Betrag',
      width: '12em',
      render: function(data, type, row) {
        if (row.amountReplacement)
          return Util.formatAmount(row.amountReplacement);
        else
          return '';
      }
    }, {
      data: 'lastCreation',
      title: 'Letzte Ausführung',
      render: function(data, type, row) {
        return moment(row.lastCreation).format('L LT');
      }
    }],
    removeHandler: function(item, draw) {
      $('#modal-delete p').html('Periodische Ausgabe <b>' + item.id + '</b> (<b>' + item.title + '</b>) wirklich löschen? Dies kann nicht rückgängig gemacht werden!');

      $('#modal-delete').modal({
        closable: false,
        onApprove: function() {
          Api.deleteRulesPeriodicById(item.id, function() {
            draw();
          }, function(e) {
            Util.errorMsg('Konnte periodische Ausgabe nicht löschen.', e);
          });
        }
      }).modal('show');
    },
    addHandler: function(draw) {
      $('#modal-periodic .header').html('Periodische Ausgabe erstellen');
      PeriodicModal.reset();

      $('#modal-periodic').modal({
        observeChanges: true,
        allowMultiple: true,
        closable: false,
        onApprove: function() {
          $('#modal-periodic .ui.form').submit();
          if (!$('#modal-periodic .ui.form').form('is valid'))
            return false;

          Api.postRulesPeriodic(PeriodicModal.save(undefined), function() {
            draw();
          }, function(e) {
            Util.errorMsg('Konnte periodische Ausgabe nicht erstellen.', e);
          });

          draw();
        },
        onVisible: function() {
          PeriodicModal.reset();
        }
      }).modal('show');
    },
    modifyHandler: function(selected, draw) {
      $('#modal-periodic .header').html('Periodische Ausgabe bearbeiten');
      PeriodicModal.load(selected);

      $('#modal-periodic').modal({
        observeChanges: true,
        allowMultiple: true,
        closable: false,
        onApprove: function() {
          $('#modal-periodic .ui.form').submit();
          if (!$('#modal-periodic .ui.form').form('is valid'))
            return false;

          Api.putRulesPeriodicById(selected.id, PeriodicModal.save(selected), function() {
            draw();
          }, function(e) {
            Util.errorMsg('Konnte periodische Ausgabe nicht bearbeiten.', e);
          });

          draw();
        },
        onVisible: function() {
          PeriodicModal.load(selected);
        }
      }).modal('show');
    },
    duplicateHandler: function(selected, draw) {
      $('#modal-periodic .header').html('Periodische Ausgabe kopieren');
      PeriodicModal.load(selected);

      $('#modal-periodic').modal({
        observeChanges: true,
        allowMultiple: true,
        closable: false,
        onApprove: function() {
          $('#modal-periodic .ui.form').submit();
          if (!$('#modal-periodic .ui.form').form('is valid'))
            return false;

          Api.postRulesPeriodic(PeriodicModal.save(undefined), function() {
            draw();
          }, function(e) {
            Util.errorMsg('Konnte periodische Ausgabe nicht erstellen.', e);
          });

          draw();
        },
        onVisible: function() {
          PeriodicModal.load(selected);
        }
      }).modal('show');
    }
  };

  table = new Table(opts);
  table.init();
  dataTable = table.table;

  PeriodicModal.init();
}

export function refresh() {
  table.refresh();
}
