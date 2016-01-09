import FastClick from 'fastclick';
import _ from 'lodash';
import Rx from 'rx';
import moment from 'moment';
import Cycle from '@cycle/core';
import { makeHTTPDriver } from '@cycle/http';
import {
  small,
  table,
  th,
  td,
  tr,
  span,
  button,
  ul,
  li,
  div,
  label,
  input,
  h1,
  h2,
  h3,
  makeDOMDriver
} from '@cycle/dom';

FastClick.attach(document.body);

const FIREBASE = 'https://olive-the-things.firebaseio.com';
const EVENTS_URL = `${FIREBASE}/events.json`;

function lastEvent(events) {
  return _.chain(events)
    .sortByOrder('time', 'desc')
    .first()
    .thru((event) => {
      if (event && event.time) {
        var t = moment(event.time);
        return `${t.fromNow()} at ${t.format('HH:mm')}`;
      } else {
        return 'none';
      }
    })
    .value();
}

function nextFeeding(events) {
  return _.chain(events)
    .sortByOrder('time', 'desc')
    .first()
    .thru((event) => {
      if (event && event.time) {
        var t = moment(event.time);
        return `next feeding between ${t.add(2, 'hours').format('HH:mm')} and ${t.add(1, 'hours').format('HH:mm')}`;
      } else {
        return 'n/a';
      }
    })
    .value();
}

function lastMedication(medicationEvents, medicationName) {
  return _.chain(medicationEvents)
    .filter('name', medicationName)
    .thru(lastEvent)
    .value();
}


function main(sources) {

  const timeshift$ = sources.DOM.select('.timeshift').events('change')
    .map((ev) => { return parseInt(ev.target.value, 10) })
    .startWith(0);

  const eventRequest$ = Rx.Observable.merge(
      Rx.Observable.interval(60000),
      sources.DOM.select('.refresh').events('click'),
      sources.HTTP.filter(res$ => res$.request.method !== 'GET') // refresh for every post
    )
    .startWith(null)
    .map(() => {
      return {
        url: EVENTS_URL,
        method: 'GET'
      };
    });

  const poopCheck$ = sources.DOM.select('.poop').events('change')
    .map(ev => ev.target.checked)
    .startWith(false);
  const peeCheck$ = sources.DOM.select('.pee').events('change')
    .map(ev => ev.target.checked)
    .startWith(false);

  const postFeed$ = sources.DOM.select('.feed').events('click')
    .withLatestFrom(
      timeshift$,
      (nope, timeshift) => { return timeshift; }
    )
    .map((timeshift) => {
      return {
        url: EVENTS_URL,
        method: 'POST',
        eager: true,
        send: {
          type: 'feeding',
          time: moment().add(timeshift, 'minutes').toISOString(),
          who: 'olive'
        }
      }
    });

  const postDiaper$ = sources.DOM.select('.diaper').events('click')
    .withLatestFrom(
      poopCheck$,
      peeCheck$,
      timeshift$,
      (nope, poop, pee, timeshift) => { return { poop, pee, timeshift }; }
    )
    .map(({ poop, pee, timeshift }) => {
      return {
        url: EVENTS_URL,
        method: 'POST',
        eager: true,
        send: {
          time: moment().add(timeshift, 'minutes').toISOString(),
          type: 'diaper',
          who: 'olive',
          poop,
          pee
        }
      };
    })

  const postTylenol$ = sources.DOM.select('.tylenol').events('click')
    .withLatestFrom(
      timeshift$,
      (nope, timeshift) => { return timeshift; }
    )
    .map((timeshift) => {
      return {
        url: EVENTS_URL,
        method: 'POST',
        eager: true,
        send: {
          time: moment().add(timeshift, 'minutes').toISOString(),
          type: 'medication',
          name: 'tylenol',
          who: 'andrea'
        }
      };
    });

  const postIbuprofen$ = sources.DOM.select('.ibuprofen').events('click')
    .withLatestFrom(
      timeshift$,
      (nope, timeshift) => { return timeshift; }
    )
    .map((timeshift) => {
      return {
        url: EVENTS_URL,
        method: 'POST',
        eager: true,
        send: {
          time: moment().add(timeshift, 'minutes').toISOString(),
          type: 'medication',
          name: 'ibuprofen',
          who: 'andrea'
        }
      };
    });

  const eventsByType$ = sources.HTTP
    .filter(res$ => res$.request.url.indexOf(EVENTS_URL) === 0 && res$.request.method === 'GET')
    .mergeAll()
    .map((res) => {
      return _.chain(res.body)
        .values()
        .sortByOrder('time', 'desc')
        .groupBy('type')
        .value()
    })
    .startWith(null);

  const vtree$ = Rx.Observable.combineLatest(
      eventsByType$,
      timeshift$,
      (eventsByType, timeshift) => { return { eventsByType, timeshift }; }
    )
    .map(({eventsByType, timeshift}) => {
      return (
        div([
          eventsByType ? [
            h3('.right', [
              span(`Refreshed at ${moment().format('HH:mm')}`),
              button('.refresh .pure-button', '↻')
            ]),
            table('.highlights .pure-table .pure-table-horizontal', [
              tr([
                th('Feeding'),
                td([
                  div(lastEvent(eventsByType.feeding)),
                  small(nextFeeding(eventsByType.feeding))
                ]),
              ]),
              tr([
                th('Tylenol'),
                td(lastMedication(eventsByType.medication, 'tylenol'))
              ]),
              tr([
                th('Ibuprofen'),
                td(lastMedication(eventsByType.medication, 'ibuprofen'))
              ]),
              tr([
                th('Diaper'),
                td(lastEvent(eventsByType.diaper))
              ])
            ]),
          ] : null,
          div('.center', [
            span(`${timeshift} min`),
            input('.timeshift', {attributes: {type: 'range', step: 5, min: -20, max: 20}}),
          ]),
          div('.buttons .right', [
            div([
              button('.feed .pure-button .pure-button-primary', 'Feed'),
            ]),
            div([
              button('.tylenol .pure-button .pure-button-primary', 'Tylenol'),
            ]),
            div([
              button('.ibuprofen .pure-button .pure-button-primary', 'Ibuprofen'),
            ]),
            div([
              label([
                input('.poop', {attributes: {type: 'checkbox'}}),
                'poop',
              ]),
              label([
                input('.pee', {attributes: {type: 'checkbox'}}),
                'pee',
              ]),
              button('.diaper .pure-button .pure-button-primary', 'Diaper'),
            ]),
          ]),
        ])
      );
    });

  const sinks = {
    DOM: vtree$,
    HTTP: Rx.Observable.merge(
      eventRequest$,
      postFeed$,
      postIbuprofen$,
      postTylenol$,
      postDiaper$
    )
  };

  return sinks;
}

Cycle.run(main, {
  DOM: makeDOMDriver('#app'),
  HTTP: makeHTTPDriver()
});
