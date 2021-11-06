function combs(str) {
    let n = str.length

    let result = []
    for (var i = 0; i < n; i++) {
        for (var j = i + 1; j <= n; j++) {
            let m = str.slice(i, j)
            result.push([m, [i, j]])
        }
    }

    return result
}

function makeColor(str, vec) {
    let n = vec.length
    let result = ''
    let curr = ''
    let color = vec[0]
    for (var i = 0; i <= n; i++) {
        if (vec[i] == color) {
            curr += str[i]
        } else {
            result +=
                color ?
                    `<mark>${curr}</mark>` :
                    curr
            color = vec[i]
            curr = str[i]
        }
    }

    return result
}

function match(str, expr) {
    return str.match(new RegExp(`^${expr}$`)) !== null
}


let timeout = null

function handleInput() {

    clearTimeout(timeout);

    timeout = setTimeout(function () {

        var text = $content.val()
        let cs = combs(text)

        app.ports.
            matchQuery.send([cs, $regex.val()]);

    }, 100);
}

function applyRegex(res) {
    let { ok, errors, result } = res

    if (!ok) {
        showError(errors)
        return;
    }

    weareGood()

    let n = result.length

    let vec = new
        Array($content.val().length).fill(false)

    let text = $content.val()

    for (var k = 0; k < n; k++) {
        let [ok, [i, j]] = result[k]

        if (!ok) continue
        for (var t = i; t < j; t++) {
            vec[t] = ok
        }
    }

    let ht = makeColor(text, vec)
    $marker.html(ht)
}

function handleScroll() {
    var scrollTop = $content.scrollTop()
    $marksBox.scrollTop(scrollTop)

    var scrollLeft = $content.scrollLeft()
    $marksBox.scrollLeft(scrollLeft)
}

function bindHandlers() {
    $content.on({
        input: handleInput,
        scroll: handleScroll,
    })

    $regex.on({
        input: handleInput,
        scroll: handleScroll,
    })

    $luckyBtn.on({
        click: () => {
            $regex.val($randExpEngine.rand())
            handleInput()
        }
    })


    $errorsBtn.on({
        click: () => {
            $errorsBtn.toggleClass("active");

            if ($errorsContent.css('display') === "block") {
                $errorsContent.css('display', "none");
                $errorsBtn.text("Show Errors")
            } else {
                $errorsContent.css('display', "block");
                $errorsBtn.text("Hide Errors")
            }
        }
    })

    $epsilonBtn.on({
        click: () => {
            $regex.insert("ε")
            handleInput()
        }
    })

    $phiBtn.on({
        click: () => {
            $regex.insert("∅")
            handleInput()
        }
    })


    $unoImg.on({
        click: () => {
            app.ports.
                reverseQuery.send($regex.val());
        }
    })
}

$.fn.insert = function (s) {
    return this.each(function () {
        let me = $(this);
        let cs = me.prop('selectionStart');
        let ce = me.prop('selectionEnd');
        let v = me.val();
        let before = v.substring(0, cs);
        let after = v.substring(ce, v.length);

        me.val(before + s + after);
        me.prop('selectionStart', cs + 1);
        me.prop('selectionEnd', cs + 1);
        me.focus();
    });
}

function showError(e) {
    $regex.addClass('invalid')
    $errorsContent.text(e)
    $errorsContent.css('color', 'red')
}

function weareGood() {
    $regex.removeClass('invalid')
    $errorsContent.text("Life's good.")
    $errorsContent.css('color', 'green')
}

function handleReverse(res) {
    let { ok, errors, result } = res

    if (!ok) {
        showError(errors)
        return;
    }

    $regex.val(result)
    handleInput()
}
