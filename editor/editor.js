"use strict";

import { JSONRPC } from './jsonrpc.js';
import * as keyevent from './keyevent.js';
import * as meaw from 'meaw';

function computeFontSize(font) {

  const canvas = document.createElement('canvas');
  const ctx = canvas.getContext('2d');
  ctx.font = font;

  const textMetrics = ctx.measureText('W');

  return [
    Math.floor(textMetrics.width),
    textMetrics.fontBoundingBoxAscent + 4 // +しているのは描画がはみ出して描画跡が残らないようにするため
  ];
}

function drawBlock({ctx, x, y, width, height, style}) {
  ctx.fillStyle = style;
  ctx.fillRect(x, y, width, height);
}

function drawText({ctx, x, y, text, font, style, option}) {
  y += 4; // 少しずらしておかないと上の部分が現在行からはみ出して、その行だけ再描画しても描画跡が残ってしまう
  ctx.fillStyle = style;
  ctx.font = font;
  ctx.textBaseline = 'top';
  for (const c of text) {
    switch (meaw.getEAW(c)) {
    case 'A':
    case 'F':
    case 'W':
      ctx.fillText(c, x, y, option.fontWidth * 2);
      x += option.fontWidth * 2;
      break;
    default:
      ctx.fillText(c, x, y, option.fontWidth);
      x += option.fontWidth;
      break;
    }
  }
}

function drawHorizontalLine({ctx, x, y, width, style, lineWidth = 1}) {
  ctx.strokeStyle = style;
  ctx.lineWidth = lineWidth;
  ctx.setLineDash = [];
  ctx.beginPath();
  ctx.moveTo(x, y);
  ctx.lineTo(x + width, y);
  ctx.stroke();
}

function drawVerticalLine({ctx, x, y, height, style, lineWidth = 1}) {
  ctx.strokeStyle = style;
  ctx.lineWidth = lineWidth;
  ctx.setLineDash = [];
  ctx.beginPath();
  ctx.moveTo(x, y);
  ctx.lineTo(x, y + height);
  ctx.stroke();
}

function drawBox({ctx, x, y, width, height, style, lineWidth = 1}) {
  ctx.strokeStyle = style;
  ctx.lineWidth = lineWidth;
  ctx.strokeRect(x, y, width, height);
}

function drawFillBox({ctx, x, y, width, height, style}) {
  ctx.fillStyle = style;
  ctx.fillRect(x, y, width, height);
}

class Option {
  constructor(fontName, fontSize) {
    const font = fontSize + 'px ' + fontName;
    this.font = font;
    const [width, height] = computeFontSize(font);
    this.fontWidth = width;
    this.fontHeight = height;
    this.foreground = '#333';
    this.background = '#ccc';
  }
}

class Surface {
  constructor(option, x, y, width, height) {
    this.option = option;
    this.x = x;
    this.y = y;
    this.width = width;
    this.height = height;
    this.drawingQueue = [];
  }

  drawBlock(x, y, width, height, color) {
    const option = this.option;
    this.drawingQueue.push(function(ctx) {
      drawBlock({
        ctx,
        x: x * option.fontWidth,
        y: y * option.fontHeight,
        width: width * option.fontWidth,
        height: height * option.fontHeight,
        style: color,
      })
    });
  }

  drawText(x, y, text, textWidth, attribute) {
    const option = this.option;
    this.drawingQueue.push(function(ctx) {
      if (!attribute) {
        drawBlock({
          ctx,
          x: x * option.fontWidth,
          y: y * option.fontHeight,
          width: textWidth * option.fontWidth,
          height: option.fontHeight,
          style: option.background,
        });
        drawText({
          ctx,
          x: x * option.fontWidth,
          y: y * option.fontHeight,
          text: text,
          style: option.foreground,
          font: option.font,
          option,
        });
      } else {
        let {foreground, background, bold, reverse, underline} = attribute;
        if (!foreground) {
          foreground = option.foreground;
        }
        if (!background) {
          background = option.background;
        }
        if (reverse) {
          const tmp = background;
          background = foreground;
          foreground = tmp;
        }
        const gx = x * option.fontWidth;
        const gy = y * option.fontHeight;
        drawBlock({
          ctx,
          x: gx,
          y: gy,
          width: textWidth * option.fontWidth,
          height: option.fontHeight,
          style: background,
        });
        drawText({
          ctx,
          x: gx,
          y: gy,
          text: text,
          style: foreground,
          font: bold ? ('bold ' + option.font) : ('normal ' + option.font),
          option,
        });
        if (underline) {
          drawHorizontalLine({
            ctx,
            x: gx,
            y: gy + option.fontHeight - 2,
            width: textWidth * option.fontWidth,
            style: foreground,
            lineWidth: 2
          });
        }
      }
    });
  }

  touch(ctx) {
    for (let fn of this.drawingQueue) {
      fn(ctx);
    }
    this.drawingQueue = [];
  }
}

class View {
  constructor(option, id, x, y, width, height, useModeline, kind) {
    this.option = option;
    this.id = id;
    this.x = x;
    this.y = y;
    this.width = width;
    this.height = height;
    this.useModeline = useModeline;
    this.kind = kind;

    this.editSurface = new Surface(option, x, y, width, height);
    this.modelineSurface = useModeline ? new Surface(option, x, y + height, width, 1) : null;
  }

  delete() {}

  resize(width, height) {
    this.width = width;
    this.height = height;
  }

  move(x, y) {
    this.x = x;
    this.y = y;
  }

  clear() {
    this.editSurface.drawBlock(
      this.x,
      this.y,
      this.width,
      this.height,
      this.option.background,
    );
  }

  clearEol(x, y) {
    this.editSurface.drawBlock(
      this.x + x,
      this.y + y,
      this.width - x,
      1,
      this.option.background,
    );
  }

  clearEob(x, y) {
    this.editSurface.drawBlock(
      this.x,
      this.y + y,
      this.width,
      this.height - y,
      this.option.background,
    );
  }

  print(x, y, text, textWidth, attribute) {
    this.editSurface.drawText(
      this.x + x,
      this.y + y,
      text,
      textWidth,
      attribute,
    );
  }

  printToModeline(x, y, text, textWidth, attribute) {
    if (this.modelineSurface) {
      this.modelineSurface.drawText(
        this.x + x,
        this.y + this.height + y,
        text,
        textWidth,
        attribute,
      );
    }
  }

  touch(ctx) {
    this.editSurface.touch(ctx);
    if (this.modelineSurface) {
      this.modelineSurface.touch(ctx);
    }
  }

  drawBorder(ctx) {
    if (this.kind === 'tile') {
      drawFillBox({
        ctx,
        x: this.x * this.option.fontWidth - this.option.fontWidth,
        y: this.y * this.option.fontHeight,
        width: this.option.fontWidth,
        height: (this.height + (this.useModeline ? 1 : 0)) * this.option.fontHeight,
        style: this.option.background,
      });
      drawVerticalLine({
        ctx,
        x: this.x * this.option.fontWidth - this.option.fontWidth / 2,
        y: this.y * this.option.fontHeight,
        height: (this.height + (this.useModeline ? 1 : 0)) * this.option.fontHeight,
        style: this.option.foreground,
      });
    } else if (this.kind === 'floating') {
      drawFillBox({
        ctx,
        x: this.x * this.option.fontWidth - this.option.fontWidth / 2,
        y: this.y * this.option.fontHeight - this.option.fontHeight / 2,
        width: (this.width + 1) * this.option.fontWidth,
        height: (this.height + 1) * this.option.fontHeight,
        style: this.option.foreground,
      });
      drawFillBox({
        ctx,
        x: this.x * this.option.fontWidth - this.option.fontWidth / 2 + 1,
        y: this.y * this.option.fontHeight - this.option.fontHeight / 2 + 1,
        width: (this.width + 1) * this.option.fontWidth - 2,
        height: (this.height + 1) * this.option.fontHeight - 2,
        style: this.option.background,
      });
    }
  }
}

export class Editor {
  constructor({
    canvas,
    fontName,
    fontSize,
    onLoaded,
    url,
    userId,
    permission,
    isOwner,
    onExit,
    onUserEnter,
    onUserExit,
    onClosed,
    onFinishGitClone,
    onRestart,
    onUserInput,
  }) {
    this.option = new Option(fontName, fontSize);
    this.canvas = canvas;
    this.canvas.width = window.innerWidth;
    this.canvas.height = window.innerHeight;

    this.onExit = onExit;
    this.onUserEnter = onUserEnter;
    this.onUserExit = onUserExit;
    this.onFinishGitClone = onFinishGitClone;
    this.onLoaded = onLoaded;
    this.onRestart = onRestart;
    this.onUserInput = onUserInput;
    this.inputEnabled = true;
    this.userId = userId;
    this.permission = permission
    this.isOwner = isOwner;

    this.messageMap = new Map();
    this.viewMap = new Map();

    const jsonrpc = new JSONRPC(url, {
      onClosed: () => {
        onClosed();
      },
    });
    this.jsonrpc = jsonrpc;

    this.registerMethod('startup', this.startup.bind(this));
    this.registerMethod('update-foreground', this.updateForeground.bind(this));
    this.registerMethod('update-background', this.updateBackground.bind(this));
    this.registerMethod('make-view', this.makeView.bind(this));
    this.registerMethod('delete-view', this.deleteView.bind(this));
    this.registerMethod('resize-view', this.resize.bind(this));
    this.registerMethod('move-view', this.move.bind(this));
    this.registerMethod('clear', this.clear.bind(this));
    this.registerMethod('clear-eol', this.clearEol.bind(this));
    this.registerMethod('clear-eob', this.clearEob.bind(this));
    this.registerMethod('put', this.put.bind(this));
    this.registerMethod('modeline-put', this.modelinePut.bind(this));
    this.registerMethod('update-display', this.updateDisplay.bind(this));
    this.registerMethod('move-cursor', this.moveCursor.bind(this));
    this.registerMethod('resize-display', this.resizeDisplay.bind(this));
    this.registerMethod('bulk', this.bulk.bind(this));
    this.registerMethod('user-enter', this.userEnter.bind(this));
    this.registerMethod('user-exit', this.userExit.bind(this));
    this.registerMethod('exit', this.exitEditor.bind(this));
    this.registerMethod('finish-git-clone', this.finishGitClone.bind(this));
    this.registerMethod('user-input', this.userInput.bind(this));

    for (const [method, handler] of this.messageMap) {
      jsonrpc.on(method, handler);
    }
    this.login(userId, permission);

    this.boundedHandleKeyDown = this.handleKeyDown.bind(this);
    this.boundedHandleResize = this.handleResize.bind(this);
  }

  initEventListeners() {
    document.addEventListener('keydown', this.boundedHandleKeyDown);
    window.addEventListener('resize', this.boundedHandleResize);
  }

  finalizeEventListeners() {
    document.removeEventListener('keydown', this.boundedHandleKeyDown);
    window.removeEventListener('resize', this.boundedHandleResize);
  }

  closeConnection() {
    this.jsonrpc.close();
  }

  handleKeyDown(event) {
    if (this.inputEnabled) {
      event.preventDefault();
      const key = keyevent.convertKeyEvent(event);
      if (key) {
        this.jsonrpc.notify('input', { kind: 1, value: key });
      }
    }
  }

  handleResize(event) {
    const canResize = true; // this.isOwner; 1 user 1 processにしたからオーナー以外もディスプレイサイズを変えられるようになった
    if (canResize) {
      this.canvas.width = event.target.innerWidth;
      this.canvas.height = event.target.innerHeight;
      this.jsonrpc.notify('redraw', {size: this.getCanvasSize()});
    } else {
      this.jsonrpc.notify('redraw');
    }
  }

  enableInput() {
    this.inputEnabled = true;
  }

  disableInput() {
    this.inputEnabled = false;
  }

  sendNotification(method, args) {
    this.jsonrpc.notify(method, args);
  }

  request(method, args, callback) {
    this.jsonrpc.request(method, args, callback);
  }

  getCanvasSize() {
    return {
      width: Math.floor(this.canvas.width / this.option.fontWidth),
      height: Math.floor(this.canvas.height / this.option.fontHeight),
    };
  }

  registerMethod(method, handler) {
    this.messageMap.set(method, handler);
  }

  callMessage(method, argument) {
    this.messageMap.get(method)(argument);
  }

  findViewById(id) {
    return this.viewMap.get(id);
  }

  login() {
    this.jsonrpc.request('login', {
      userId: this.userId,
      permission: {
        write: this.permission.write,
        read: this.permission.read,
        resizeDisplay: this.permission.resizeDisplay,
      },
      size: this.getCanvasSize(),
      foreground: this.option.foreground,
      background: this.option.background,
      isOwner: this.isOwner,
    }, (response) => {
      console.log(response);
      this.updateForeground(response.foreground);
      this.updateBackground(response.background);
      if (!this.isOwner) {
        this.canvas.width = response.size.width * this.option.fontWidth;
        this.canvas.height = response.size.height * this.option.fontHeight;
      }
      if (response.views) {
        for (const view of response.views) {
          this.makeView(view);
        }
      }
      if (this.isOwner) {
        this.jsonrpc.notify('redraw', {size: this.getCanvasSize()});
      } else {
        this.jsonrpc.notify('redraw', {});
      }
    });
  }

  startup() {
    console.log('startup');
    if (this.onRestart) {
      this.onRestart();
    }
  }

  updateForeground(color) {
    this.option.foreground = color;
  }

  updateBackground(color) {
    this.option.background = color;
    this.canvas.style.backgroundColor = color;
  }

  makeView({ id, x, y, width, height, use_modeline, kind }) {
    const view = new View(this.option, id, x, y, width, height, use_modeline, kind);
    this.viewMap.set(id, view);
  }

  deleteView({ viewInfo: { id }}) {
    const view = this.findViewById(id);
    view.delete();
    this.viewMap.delete(id);
  }

  resize({ viewInfo: { id }, width, height }) {
    const view = this.findViewById(id);
    view.resize(width, height);
  }

  move({ viewInfo: { id }, x, y }) {
    const view = this.findViewById(id);
    view.move(x, y);
  }

  clear({ viewInfo: { id }}) {
    const view = this.findViewById(id);
    view.clear();
  }

  clearEol({ viewInfo: { id }, x, y }) {
    const view = this.findViewById(id);
    view.clearEol(x, y);
  }

  clearEob({ viewInfo: { id }, x, y }) {
    const view = this.findViewById(id);
    view.clearEob(x, y);
  }

  put({ viewInfo: { id }, x, y, text, textWidth, attribute }) {
    const view = this.findViewById(id);
    view.print(x, y, text, textWidth, attribute);
  }

  modelinePut({ viewInfo: { id }, x, y, text, textWidth, attribute }) {
    const view = this.findViewById(id);
    view.printToModeline(x, y, text, textWidth, attribute);
  }

  updateDisplay() {
    const ctx = this.canvas.getContext('2d');
    for (const [id, view] of this.viewMap) {
      if (view.kind === 'tile') {
        view.drawBorder(ctx);
        view.touch(ctx);
      }
    }
    for (const [id, view] of this.viewMap) {
      if (view.kind === 'floating') {
        view.drawBorder(ctx);
        view.touch(ctx);
      }
    }
  }

  moveCursor() {}

  resizeDisplay(size) {
    this.canvas.width = this.option.fontWidth * size.width;
    this.canvas.height = this.option.fontHeight * size.height;
  }

  bulk(messages) {
    if (this.onLoaded) {
      this.onLoaded();
      this.onLoaded = null;
    }
    for (const {method, argument} of messages) {
      this.callMessage(method, argument);
    }
  }

  userEnter({userId}) {
    if (this.onUserEnter) {
      this.onUserEnter(userId);
    }
  }

  userExit({userId}) {
    if (this.onUserExit) {
      this.onUserExit(userId);
    }
  }

  exitEditor() {
    if (this.onExit) {
      this.onExit();
    }
  }

  finishGitClone({url, success, errorMessage}) {
    if (this.onFinishGitClone) {
      this.onFinishGitClone(url, success, errorMessage);
    }
  }

  userInput({userId, value}) {
    if (this.onUserInput) {
      this.onUserInput(userId, value);
    }
  }
}
