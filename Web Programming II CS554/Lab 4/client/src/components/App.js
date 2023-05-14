import React from 'react';
import logo from '../images/marvel-logo.png';
import '../App.css';
import { BrowserRouter as Router, Link, Routes, Route } from 'react-router-dom';
import Home from './Home';
import Characters from './Characters';
import Comics from './Comics';
import Stories from './Stories';
import CharacterDetails from './CharacterDetails';
import ComicDetails from './ComicDetails';
import StoriesDetails from './StoriesDetails';
import CharacterHistory from './CharacterHistory';
import Error from './Error';

function App() {
  return (
    <Router>
      <div className="App">
        <header className="App-header">
          <div className="App-logo-container">
            <img src={logo} className="App-logo" alt="logo" />
          </div>
          <div className='App-nav-container'>
            <ul className="App-nav">
              <li><Link className='contentLink' to='/'>
                Home
              </Link></li>
              <li><Link className='contentLink' to='/characters/page/1'>
                Characters
              </Link></li>
              <li><Link className='contentLink' to='/comics/page/1'>
                Comics
              </Link></li>
              <li> <Link className='contentLink' to='/stories/page/1'>
                Stories
              </Link></li>
              <li><Link className='contentLink' to='/characters/history'>
                History
              </Link></li>
            </ul>
          </div>
        </header>
        <div className="App-body">
          <Routes>
            <Route path='/' element={<Home />}></Route>
            <Route path='/characters/page/:pagenum' element={<Characters />} />
            <Route path='/comics/page/:pagenum' element={<Comics />} />
            <Route path='/stories/page/:pagenum' element={<Stories />} />
            <Route path='/characters/:id' element={<CharacterDetails />} />
            <Route path='/comics/:id' element={<ComicDetails />} />
            <Route path='/stories/:id' element={<StoriesDetails />} />
            <Route path='/characters/history' element={<CharacterHistory />} />
            <Route path='/*' element={<Error />} />
          </Routes>
        </div>
      </div>
    </Router>
  );
}

export default App;