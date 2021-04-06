'use strict';

import moment from 'moment/src/moment.js';
import $ from '/jquery.js';
import * as Util from '/util.js';
import * as Common from '/common.js';
import * as Sunburst from './sunburst.js';

export let location;
let table;

let formatDataSource = function(x) {
  if (typeof x.base != 'undefined' && typeof x.steps != undefined)
    return moment(x.base.date).format('L LT') + ' ' + Util.formatAdditions(x.steps);
  else
    return '';
};

let showDetails = function() {
  let tr = $(this).closest('tr');
  let row = table.row(tr);

  if (row.child.isShown()) {
    row.child().find('#table-ovdetails').DataTable().destroy();
    row.child.hide();
    tr.find('a').first().html('<i class="plus circle icon"></i>');

    tr.removeClass('shown');
  } else {
    row.child('<div class="ui grid"><div class="row"><div class="sixteen wide column"><table id="table-ovdetails" class="ui celled table mytable" width="100%" cellspacing="0"></table></div></div>' +
      '<div class="row"><div id="incomebycat" class="seven wide column"></div>' +
      '<div id="expsbycat" class="seven wide column"></div>' +
      '</div></div>'
    ).show();
    tr.find('a').first().html('<i class="minus circle icon"></i>');

    Sunburst.createSmall(row.child().find('#expsbycat'), Sunburst.categoryData(row.data().statsExps, 'Ausgaben'));
    Sunburst.createSmall(row.child().find('#incomebycat'), Sunburst.categoryData(row.data().statsIncome, 'Einnahmen'));

    let childData = [];
    for (let i = 0; i < row.data().startBalances.balances.length; i++) {
      let sb = row.data().startBalances.balances[i];
      let se = row.data().endBalances.balances[i];
      let ses = row.data().endBalancesApprox.balances[i];
      let a = Common.findAccount(sb.accountId);

      childData.push({
        account: a.title,
        accountColor: a.color,
        start: sb,
        end: se,
        endApprox: ses,
        startAmount: sb.amount,
        endAmount: se.amount,
        endAmountApprox: ses.amount,
        startData: sb,
        endData: se,
        endDataApprox: ses,
        error: Math.abs(ses.amount - se.amount),
        delta: se.amount - sb.amount,
        count: ses.steps - sb.steps
      });
    }

    let childTable = row.child().find('#table-ovdetails');
    childTable.DataTable({
      data: childData,
      'paging': false,
      'ordering': false,
      'info': false,
      'dom': 't',
      'language': Util.datatableLanguage,
      createdRow: function(row, data) {
        $(row).find('td:nth-child(n+4)').addClass('right aligned');
        Util.setBackgroundColor($(row).find('td:nth-child(1)'), data.accountColor, false);

        $(row).find('td:nth-child(4)').addClass(isNaN(data.startAmount) ? 'warning' : (data.startAmount >= 0 ? '' : 'negative'));
        $(row).find('td:nth-child(5)').addClass(isNaN(Util.nullToNaN(data.endAmount)) ? 'warning' : (data.endAmount >= 0 ? '' : 'negative'));
        $(row).find('td:nth-child(6)').addClass(isNaN(Util.nullToNaN(data.endAmountApprox)) ? 'warning' : (data.endAmountApprox >= 0 ? '' : 'negative'));

        $(row).find('td:nth-child(8)').addClass(isNaN(data.error) ? 'warning' : (data.error >= 50 ? 'negative' : (data.error >= 25 ? 'warning' : 'positive')));
        $(row).find('td:nth-child(9)').addClass(isNaN(data.delta) ? 'warning' : (data.delta < 0 ? 'negative' : 'positive'));
      },
      columns: [{
        data: 'account',
        title: 'Konto'
      }, {
        data: 'start',
        title: 'Datenquelle Beginn',
        width: '16em',
        render: function(data) {
          return formatDataSource(data);
        }
      }, {
        data: 'end',
        title: 'Datenquelle Ende',
        width: '16em',
        render: function(data) {
          return formatDataSource(data);
        }
      }, {
        data: 'startAmount',
        title: 'Stand zu Beginn',
        width: '8em',
        render: function(data) {
          return Util.formatAmount(data);
        }
      }, {
        data: 'endAmount',
        title: 'Stand am Ende',
        width: '8em',
        render: function(data) {
          return Util.formatAmount(data);
        }
      }, {
        data: 'endAmountApprox',
        title: 'Schätzung am Ende',
        width: '9em',
        render: function(data) {
          return Util.formatAmount(data);
        }
      }, {
        data: 'count',
        title: 'Ausgaben',
        width: '6em'
      }, {
        data: 'error',
        title: 'Integrationsfehler',
        width: '8em',
        render: function(data) {
          return Util.formatAmount(data);
        }
      }, {
        data: 'delta',
        title: 'Delta',
        width: '6em',
        render: function(data) {
          return Util.formatAmount(data);
        }
      }]
    });

    tr.addClass('shown');
  }
};

export function refresh(d) {
  table.clear().rows.add(d).draw();
}

export function init(loc) {
  location = $(loc);
  table = location.DataTable({
    'paging': false,
    'ordering': false,
    'info': false,
    'dom': 't',
    'data': [],
    'language': Util.datatableLanguage,
    createdRow: function(row, data) {
      $(row).find('td').slice(2).addClass('right aligned');

      $(row).find('td').first().addClass('center aligned').addClass('selectable');
      $(row).find('a').first().on('click', showDetails);

      $(row).find('td').last().html('');
      $(row).find('td:nth-child(3)').addClass(isNaN(Util.nullToNaN(data.moneyStart)) ? 'warning' : (data.moneyStart >= 0 ? '' : 'negative'));
      $(row).find('td:nth-child(4)').addClass(isNaN(Util.nullToNaN(data.moneyEnd)) ? 'warning' : (data.moneyEnd >= 0 ? '' : 'negative'));
      $(row).find('td:nth-child(5)').addClass(isNaN(Util.nullToNaN(data.moneyEndApprox)) ? 'warning' : (data.moneyEndApprox >= 0 ? '' : 'negative'));
      $(row).find('td:nth-child(7)').addClass(isNaN(data.moneyError) ? 'warning' : (data.moneyError >= 50 ? 'negative' : (data.moneyError >= 25 ? 'warning' : 'positive')));
      $(row).find('td:nth-child(8)').addClass(isNaN(data.moneyDelta) ? 'warning' : (data.moneyDelta < 0 ? 'negative' : (data.moneyDelta < 100 ? 'warning' : 'positive')));
    },
    columns: [{
      data: null,
      defaultContent: '<a href="javascript:void(0)"><i class="plus circle icon"></i></a>',
      width: '2em'
    }, {
      data: 'timespan',
      title: 'Zeitspanne',
      width: '12em'
    }, {
      data: 'moneyStart',
      title: 'Vermögen zu Beginn',
      width: '10em',
      render: function(data) {
        return Util.formatAmount(data);
      }
    }, {
      data: 'moneyEnd',
      title: 'Vermögen am Ende',
      width: '9em',
      render: function(data) {
        return Util.formatAmount(data);
      }
    }, {
      data: 'moneyEndApprox',
      title: 'Schätzung am Ende',
      width: '9.5em',
      render: function(data) {
        return Util.formatAmount(data);
      }
    }, {
      data: 'expenseCount',
      title: 'Ausgaben',
      width: '6em'
    }, {
      data: 'moneyError',
      title: 'Integrationsfehler',
      width: '8em',
      render: function(data) {
        return Util.formatAmount(data);
      }
    }, {
      data: 'moneyDelta',
      title: 'Delta',
      width: '6em',
      render: function(data) {
        return Util.formatAmount(data);
      }
    }, {
      data: null,
      title: ''
    }]
  }).on('draw.dt', function() {
    $(this).find('th').addClass('center aligned');
  });
}
