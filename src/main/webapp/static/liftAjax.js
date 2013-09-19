(function() {

    window.liftAjax = {
        lift_ajaxQueue: [],
        lift_ajaxInProcess: null,
        lift_doCycleQueueCnt: 0,
        lift_ajaxShowing: false,
        lift_ajaxRetryCount: 3,

        lift_ajaxHandler: function(theData, theSuccess, theFailure, responseType){
            var toSend = {retryCnt: 0};
            toSend.when = (new Date()).getTime();
            toSend.theData = theData;
            toSend.onSuccess = theSuccess;
            toSend.onFailure = theFailure;
            toSend.responseType = responseType;
            toSend.version = liftAjax.lift_ajaxVersion++;

            // Make sure we wrap when we hit JS max int.
            var version = liftAjax.lift_ajaxVersion
            if ((version - (version + 1) != -1) || (version - (version - 1) != 1))
                liftAjax.lift_ajaxVersion = 0;

            if (liftAjax.lift_uriSuffix) {
                theData += '&' + liftAjax.lift_uriSuffix;
                toSend.theData = theData;
                liftAjax.lift_uriSuffix = undefined;
            }

            liftAjax.lift_ajaxQueue.push(toSend);
            liftAjax.lift_ajaxQueueSort();
            liftAjax.lift_doCycleQueueCnt++;
            liftAjax.lift_doAjaxCycle();
            return false; // buttons in forms don't trigger the form

        },

        knownPromises: {},

        randStr: function() {
            return Math.floor((1 + Math.random()) * 0x10000).toString(16).substring(1);},

        makeGuid: function() {return this.randStr() + this.randStr() + '-' + this.randStr() + '-' + this.randStr() + '-' +
            this.randStr() + '-' + this.randStr() + this.randStr() + this.randStr();},

        Promise: function() {
            return {
                guid: liftAjax.makeGuid(),
                "_values": [],
                '_events': [],
                '_failMsg': "",
                '_valueFuncs': [],
                '_doneFuncs': [],
                '_failureFuncs': [],
                '_eventFuncs': [],
                '_done': false,
                '_failed': false,
                processMsg: function(evt) {if (this._done || this._failed) return;
                    this._events.push(evt);
                    for (var v in this._eventFuncs) {try {this._eventFuncs[v](evt);} catch (e) {liftAjax.lift_defaultLogError(e);}};
                    if (evt.done) {this.doneMsg();} else if (evt.success) {this.successMsg(evt.success);} else if (evt.failure) {this.failMsg(evt.failure);}},
                successMsg: function(value) {if (this._done || this._failed) return; this._values.push(value); for (var f in this._valueFuncs) {this._valueFuncs[f](value);}},
                failMsg: function(msg) {if (this._done || this._failed) return; liftAjax._removeIt(this.guid); this._failed = true; this._failMsg = msg; for (var f in this._failureFuncs) {this._failureFuncs[f](msg);}},
                doneMsg: function() {if (this._done || this._failed) return; liftAjax._removeIt(this.guid); this._done = true; for (var f in this._doneFuncs) {this._doneFuncs[f]();}},
                then: function(f) {this._valueFuncs.push(f); for (var v in this._values) {try {f(this._values[v]);} catch (e) {liftAjax.lift_defaultLogError(e);;}} return this;},
                fail: function(f) {this._failureFuncs.push(f); if (this._failed) {try {f(this._failMsg);} catch (e) {liftAjax.lift_defaultLogError(e);;}}; return this;},
                done: function(f) {this._doneFuncs.push(f); if (this._done) {try {f();} catch (e) {liftAjax.lift_defaultLogError(e);;}} return this;},
                onEvent: function(f) {this._eventFuncs.push(f); for (var v in this._events) {try {f(this._events[v]);} catch (e) {liftAjax.lift_defaultLogError(e);;}}; return this;},
                map: function(f) {var ret = new liftAjax.Promise(); this.done(function() {ret.doneMsg();}); this.fail(function (m) {ret.failMsg(m);}); this.then(function (v) {ret.successMsg(f(v));}); return ret;}
            };
        },

        _removeIt: function(g) {this.knownPromises[g] = undefined;},

        sendEvent: function(g, evt) {
            var p = this.knownPromises[g];
            if (p) {
                p.processMsg(evt);
            }
        },

        associate: function(promise) {this.knownPromises[promise.guid] = promise;},

        lift_uriSuffix: undefined,

        lift_logError: function(msg) {
            liftAjax.lift_defaultLogError(msg);
        },

        lift_defaultLogError: function(msg) {
            if (console && typeof console.error == 'function')
                console.error(msg);
            else
                alert(msg);
        },

        lift_ajaxQueueSort: function() {
            liftAjax.lift_ajaxQueue.sort(function (a, b) {return a.when - b.when;});
        },

        lift_defaultFailure: function() {
            alert("The server cannot be contacted at this time");
        },

        lift_startAjax: function() {
            liftAjax.lift_ajaxShowing = true;
            jQuery('#'+"ajax-spinner").show();
        },

        lift_endAjax: function() {
            liftAjax.lift_ajaxShowing = false;
            jQuery('#'+"ajax-spinner").hide();
        },

        lift_testAndShowAjax: function() {
            if (liftAjax.lift_ajaxShowing && liftAjax.lift_ajaxQueue.length == 0 && liftAjax.lift_ajaxInProcess == null) {
                liftAjax.lift_endAjax();
            } else if (!liftAjax.lift_ajaxShowing && (liftAjax.lift_ajaxQueue.length > 0 || liftAjax.lift_ajaxInProcess != null)) {
                liftAjax.lift_startAjax();
            }
        },

        lift_traverseAndCall: function(node, func) {
            if (node.nodeType == 1) func(node);
            var i = 0;
            var cn = node.childNodes;

            for (i = 0; i < cn.length; i++) {
                liftAjax.lift_traverseAndCall(cn.item(i), func);
            }
        },

        lift_successRegisterGC: function() {
            setTimeout("liftAjax.lift_registerGC()", 75000);
        },

        lift_failRegisterGC: function() {
            setTimeout("liftAjax.lift_registerGC()", 15000);
        },

        lift_registerGC: function() {
            var data = "__lift__GC=_",
                version = null;
            jQuery.ajax({ url : liftAjax.addPageNameAndVersion("/ajax_request/", version), data : data, type : "POST", dataType : "script", timeout : 5000, cache : false, success : liftAjax.lift_successRegisterGC, error : liftAjax.lift_failRegisterGC });
        },


        lift_sessionLost: function() {
            location.reload();
        },

        lift_doAjaxCycle: function() {
            if (liftAjax.lift_doCycleQueueCnt > 0) liftAjax.lift_doCycleQueueCnt--;
            var queue = liftAjax.lift_ajaxQueue;
            if (queue.length > 0) {
                var now = (new Date()).getTime();
                if (liftAjax.lift_ajaxInProcess == null && queue[0].when <= now) {
                    var aboutToSend = queue.shift();

                    liftAjax.lift_ajaxInProcess = aboutToSend;

                    var successFunc = function(data) {
                        liftAjax.lift_ajaxInProcess = null;
                        if (aboutToSend.onSuccess) {
                            aboutToSend.onSuccess(data);
                        }
                        liftAjax.lift_doCycleQueueCnt++;
                        liftAjax.lift_doAjaxCycle();
                    };

                    var failureFunc = function() {
                        liftAjax.lift_ajaxInProcess = null;
                        var cnt = aboutToSend.retryCnt;
                        if (arguments.length == 3 && arguments[1] == 'parsererror') {
                            liftAjax.lift_logError('The server call succeeded, but the returned Javascript contains an error: '+arguments[2])
                        } else

                        if (cnt < liftAjax.lift_ajaxRetryCount) {
                            aboutToSend.retryCnt = cnt + 1;
                            var now = (new Date()).getTime();
                            aboutToSend.when = now + (1000 * Math.pow(2, cnt));
                            queue.push(aboutToSend);
                            liftAjax.lift_ajaxQueueSort();
                        } else {
                            if (aboutToSend.onFailure) {
                                aboutToSend.onFailure();
                            } else {
                                liftAjax.lift_defaultFailure();
                            }
                        }
                        liftAjax.lift_doCycleQueueCnt++;
                        liftAjax.lift_doAjaxCycle();
                    };

                    if (aboutToSend.responseType != undefined &&
                        aboutToSend.responseType != null &&
                        aboutToSend.responseType.toLowerCase() === "json") {
                        liftAjax.lift_actualJSONCall(aboutToSend.theData, successFunc, failureFunc);
                    } else {
                        var theData = aboutToSend.theData,
                            version = aboutToSend.version;

                        liftAjax.lift_actualAjaxCall(theData, version, successFunc, failureFunc);
                    }
                }
            }

            liftAjax.lift_testAndShowAjax();
            if (liftAjax.lift_doCycleQueueCnt <= 0) liftAjax.lift_doCycleIn200()
        },

        lift_doCycleIn200: function() {
            liftAjax.lift_doCycleQueueCnt++;
            setTimeout("liftAjax.lift_doAjaxCycle();", 200);
        },

        lift_ajaxVersion: 0,

        addPageNameAndVersion: function(url, version) {

            var replacement = 'ajax_request/'+lift_page;
            if (version!=null)
                replacement += ('-'+version.toString(36)) + (liftAjax.lift_ajaxQueue.length > 35 ? 35 : liftAjax.lift_ajaxQueue.length).toString(36);
            return url.replace('ajax_request', replacement);
        },

        lift_actualAjaxCall: function(data, version, onSuccess, onFailure) {
            jQuery.ajax({ url : liftAjax.addPageNameAndVersion("/ajax_request/", version), data : data, type : "POST", dataType : "script", timeout : 5000, cache : false, success : onSuccess, error : onFailure });
        },

        lift_actualJSONCall: function(data, onSuccess, onFailure) {
            var version = null;
            jQuery.ajax({ url : liftAjax.addPageNameAndVersion("/ajax_request/", version), data : data, type : "POST", dataType : "json", timeout : 5000, cache : false, success : onSuccess, error : onFailure });
        }
    };

    window.liftUtils = {
        lift_blurIfReturn: function(e) {
            var code;
            if (!e) var e = window.event;
            if (e.keyCode) code = e.keyCode;
            else if (e.which) code = e.which;

            var targ;

            if (e.target) targ = e.target;
            else if (e.srcElement) targ = e.srcElement;
            if (targ.nodeType == 3) // defeat Safari bug
                targ = targ.parentNode;
            if (code == 13) {targ.blur(); return false;} else {return true;};
        }
    };


})();
jQuery(document).ready(function() {liftAjax.lift_doCycleIn200();});