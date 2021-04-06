'use strict';

import Table from '/table.js';
import moment from 'moment/src/moment.js';
import $ from '/jquery.js';
import * as Util from '/util.js';
import * as Api from '/moneydb.js';
import * as StockModal from './stocks_modal.js';
import _ from 'lodash';

export let table;
export let location;
export let dataTable;

export function init(loc) {
  location = $(loc);

  let opts = {
    url: 'stocks',
    location: location,
    defaultOrder: [
      [1, 'asc'],
      [0, 'asc']
    ],
    lengths: [25, 50],
    expandable: true,
    formatter: function(data) {
      let s = '<div class="ui relaxed divided list">';

      if (data.info.managingCompany != '')
        s +=
        '<div class="item"><div class="content"><div class="header">Herausgeber</div>' +
        '<div class="description">' + data.info.managingCompany + ' (' + data.info.country + ')</div></div></div>';

      if (data.info.wkn != '')
        s += '<div class="item"><div class="content"><div class="header">WKN</div>' +
        '<div class="description">' + data.info.wkn + '</div></div></div>';

      if (data.info.lastUpdate != '') {
        s += '<div class="item"><div class="content"><div class="header">Letzte Datenupdates</div>' +
          '<div class="description">';

        let m = moment(data.info.lastHistoricalUpdate);
        s += 'Historisch: ' + m.fromNow() + ', am ' + m.format('LL') + ' um ' + m.format('LT');

        m = moment(data.info.lastCurrentUpdate);
        s += '<br />Realtime: ' + m.fromNow() + ', am ' + m.format('LL') + ' um ' + m.format('LT');

        s += '</div></div></div>';
      }

      if (data.exchanges.length > 0) {
        s += '<div class="item"><div class="content"><div class="header">Handelsplätze</div><ul>';

        data.exchanges.filter(x => x.recordCount > 0).sort((b, a) => a.recordCount - b.recordCount).forEach(x => {
          s += '<li><b>' + x.name + '</b>&nbsp;&nbsp;(' + x.recordCount + ' Datensätze von ' + moment.utc(x.firstRecord.day).format('L') + ' bis ' + moment.utc(x.lastRecord.day).format('L');

          if (x.lastRealtime !== null)
            s += ', Realtime: ' + moment.utc(x.lastRealtime.date).format('L LT');

          s += ')</li>';
        });

        let others = data.exchanges.filter(x => x.recordCount == 0);

        if (others.length > 0)
          s += '<li> und ' + others.length + ' weitere ohne historische Daten</li>';

        // s += '<li>' + data.exchanges.filter(x => x.recordCount >= 0).map(x =>
        //   x.name).join(', ') + '</li>';

        s += '</ul>';
        s += '</div></div>';
      }

      s += '</div>';

      return s;
    },
    isReadOnly: () => false,
    isDeletable: () => true,
    createdRow: function() {},
    preprocess: function(row) {
      if (row.info == null)
        row.info = { id: -1, title: '', kind: '', managingCompany: '', onvistaUrl: '', wkn: '' };
      else
        row.info = Util.flattenRecord(row.info);

      row.exchanges = _.map(row.exchanges, e => {
        e.data.firstRecord = Util.flattenRecord(e.data.firstRecord);
        e.data.lastRecord = Util.flattenRecord(e.data.lastRecord);
        e.data.lastRealtime = Util.flattenRecord(e.data.lastRealtime);

        return Util.flattenRecord(e);
      });

      return row;
    },
    colorRow: () => {},
    colorRowDark: () => {},
    columns: [{
      data: 'id',
      title: 'ID',
      width: '2.5em'
    }, {
      data: 'isin',
      title: 'ISIN',
      width: '8em'
    }, {
      data: 'info.title',
      title: 'Name',
      width: '',
      orderable: false
    }, {
      data: 'info.kind',
      title: 'Art',
      width: '8em',
      orderable: false
    }, {
      data: 'info.onvistaUrl',
      title: '',
      width: '2em',
      orderable: false,
      render: function(data) {
        if (data == '')
          return '';
        else
          return '<a alt="auf onvista ansehen" target="_blank" href="https://www.onvista.de' + data + '"><i class="external icon"></i></a>';
      }
    }],
    removeHandler: function(item, draw) {
      $('#modal-delete p').html('Beobachtetes Wertpapier <b>' + item.id + '</b> (<b>' + item.isin + '</b>) wirklich löschen? Dies kann nicht rückgängig gemacht werden!');
      $('#modal-delete').modal({
        closable: false,
        onApprove: function() {
          Api.deleteStocksById(item.id, function() {
            draw();
          }, function(e) {
            Util.errorMsg('Konnte Wertpapier nicht löschen.', e);
          });
        }
      }).modal('show');
    },
    addHandler: function(draw) {
      $('#modal-stocks .header').html('Wertpapier erstellen');
      StockModal.reset();

      $('#modal-stocks').modal({
        observeChanges: true,
        allowMultiple: true,
        closable: false,
        onApprove: function() {
          $('#modal-stocks .ui.form').submit();
          if (!$('#modal-stocks .ui.form').form('is valid'))
            return false;

          Api.postStocks(StockModal.save(undefined), function() {
            draw();
          }, function(e) {
            Util.errorMsg('Konnte Wertpapier nicht erstellen.', e);
          });

          draw();
        },
        onVisible: function() {
          StockModal.reset();
        }
      }).modal('show');
    },
    modifyHandler: function(selectedStock, draw) {
      $('#modal-stocks .header').html('Wertpapier bearbeiten');
      StockModal.load(selectedStock);

      $('#modal-stocks').modal({
        observeChanges: true,
        allowMultiple: true,
        closable: false,
        onApprove: function() {
          $('#modal-stocks .ui.form').submit();
          if (!$('#modal-stocks .ui.form').form('is valid'))
            return false;

          Api.putStocksById(selectedStock.id, StockModal.save(selectedStock), function() {
            draw();
          }, function(e) {
            Util.errorMsg('Konnte Wertpapier nicht bearbeiten.', e);
          });

          draw();
        },
        onVisible: function() {
          StockModal.load(selectedStock);
        }
      }).modal('show');
    },
    duplicateHandler: function(selectedStock, draw) {
      $('#modal-stocks .header').html('Wertpapier kopieren');
      StockModal.load(selectedStock);

      $('#modal-stocks').modal({
        observeChanges: true,
        allowMultiple: true,
        closable: false,
        onApprove: function() {
          $('#modal-stocks .ui.form').submit();
          if (!$('#modal-stocks .ui.form').form('is valid'))
            return false;

          Api.postStocks(StockModal.save(undefined), function() {
            draw();
          }, function(e) {
            Util.errorMsg('Konnte Wertpapier nicht erstellen.', e);
          });

          draw();
        },
        onVisible: function() {
          StockModal.load(selectedStock);
        }
      }).modal('show');
    }
  };

  table = new Table(opts);
  table.init();
  dataTable = table.table;

  StockModal.init();
}

export function refresh() {
  table.refresh();
}
