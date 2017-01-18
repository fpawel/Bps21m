
function addRecord(date, time, level, text) {
    var x = document.createElement('span');
    x.className = "date";
    x.innerHTML = date;
    document.body.appendChild(x);

    x = document.createElement('span');
    x.className = "time";
    x.innerHTML = time;
    document.body.appendChild(x);

    x = document.createElement('span');
    x.className = level;
    x.innerHTML = text;
    document.body.appendChild(x);

    x = document.createElement('br');
    document.body.appendChild(x);
}

