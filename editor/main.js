import { Editor } from './editor.js';

const canvas = document.querySelector('#editor');

const generateUserId = () => {
  let userId = localStorage.getItem('user-id');
  if (!userId) {
    userId = crypto.randomUUID();
    localStorage.setItem('user-id', userId);
  }
  return userId;
};

const userId = generateUserId();

console.log('userId:', userId);

async function hoge() {
  //const response = await fetch(`http://localhost:50001/login?userId=${userId}`, {
  //  'content-type': 'application/json',
  //  'method': 'GET'
  //});
  //const foo = await response.json();
  //console.log(foo);

  const editor = new Editor({
    canvas: canvas,
    fontName: 'monospace',
    fontSize: 20,
    onLoaded: null,
    //url: foo['websocket-url'],
    url: 'ws://localhost:50000',
    userId: userId,
    permission: {
      read: true,
      write: true,
      resizeDisplay: true,
    },
    isOwner: true,
    onExit: null,
    onUserEnter: null,
    onUserExit: null,
    onClosed: null,
    onFinishGitClone: null,
    onRestart: null,
    onUserInput: null,
  });

  editor.initEventListeners();
}

hoge();
