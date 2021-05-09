//
// isearch.js
//
//   Copyright (C) 2004-2006 Kazuki Tsujimoto, All rights reserved.
//
//   Redistribution and use in source and binary forms, with or without
//   modification, are permitted provided that the following conditions
//   are met:
//
//   1. Redistributions of source code must retain the above copyright
//      notice, this list of conditions and the following disclaimer.
//
//   2. Redistributions in binary form must reproduce the above copyright
//      notice, this list of conditions and the following disclaimer in the
//      documentation and/or other materials provided with the distribution.
//
//   3. Neither the name of the authors nor the names of its contributors
//      may be used to endorse or promote products derived from this
//      software without specific prior written permission.
//
//   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
//   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
//   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
//   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
//   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
//   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
//   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
//   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
//   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
//   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
//   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//

var cache;
var cursor;
var prev_pattern;

function initCache() {
    cache = new Array;
    var trs = document.getElementById('all').getElementsByTagName('a');
    var length = trs.length;
    for (var i = 0; i < length; i++) {
        cache.push(trs[i]);
    }
}

function handle_magic_space(str) {
    return str.replace(/^ /, '^').replace(/ /g, '.*');
}

function grep(pattern) {
    try {
        if (prev_pattern != pattern) {
            var regex = new RegExp(handle_magic_space(pattern), "i");
            updateMatchedTable(getMatched(regex));
            prev_pattern = pattern;
        }
    } catch (e) {
    }
}

function getMatched(re) {
    if (!cache) {
        initCache();
    }
    var matched = new Array;
    var length = cache.length;
    var max = Number(document.getElementById('max').value);
    for (var i = 0; i < length; i++) {
        var e = cache[i];
        if (matched.length >= max) {
            break;
        }
        if (e.innerHTML.match(re)) {
            matched.push(e);
        }
    }
    return matched;
}

function updateMatchedTable(matched) {
    var table = document.getElementById('matched');
    var tbody = document.createElement("tbody");
    for (var i = 0; i < matched.length; i++) {
        tbody.appendChild(matched[i]);
        tbody.appendChild(document.createElement("br"));
    }
    table.replaceChild(tbody, table.getElementsByTagName('tbody')[0]);
    cursor = new Cursor(tbody);
    cursor.refresh();
}

function toggleShowPageName(show_p) {
    var length = cache.length;
    for (var i = 0; i < length; i++) {
        cache[i].getElementsByTagName('td')[2].style.display = (show_p) ? 'block' : 'none';
    }
}

function Cursor(tbody) {
    this.pos = (tbody.childNodes.length > 0) ? 0 : null;
    this.tbody = tbody;
}

Cursor.prototype.refresh = function () {
    var nodes = this.tbody.childNodes;
    var length = nodes.length;
    for (var i = 0; i < length; i++) {
       nodes[i].style.backgroundColor = '';
    }
    if (this.pos != null) {
        nodes[this.pos].style.backgroundColor = '#ccccff';
    }
}

Cursor.prototype.move = function (d) {
    if (this.pos != null &&
        0 <= this.pos + d && this.pos + d < this.tbody.childNodes.length) {
        this.pos += d;
        this.refresh();
    }
}

Cursor.prototype.down = function () {
    this.move(1);
}

Cursor.prototype.up = function () {
    this.move(-1);
}

Cursor.prototype.open = function () {
    if (this.pos != null) {
        var a = this.tbody.childNodes[this.pos].getElementsByTagName('a')[0];
        open(a.href, a.target);
    }
}

var KEY_ENTER = 13;
var KEY_UP = 38;
var KEY_DOWN = 40;

function isearchHandler(e) {
    switch (e.keyCode) {
    case KEY_ENTER:
        cursor.open();
        break;
    case KEY_UP:
        cursor.up();
        break;
    case KEY_DOWN:
        cursor.down();
        break;
    default:
        return true;
    }
    return false;
}
