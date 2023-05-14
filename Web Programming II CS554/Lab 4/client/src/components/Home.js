import { Link } from "react-router-dom";
import Button from 'react-bootstrap/Button';

function Home() {
    return (
        <div className="homeComponent">
            <h1>Welcome to the React Marvel API</h1>
            <br />
            <p>This application uses React to display catalogs of characters, stories, and comics within the Marvel Universe.
                <br />
                Try it out by using the navigation links below or in the header.
            </p>
            <Link className='navButton' to='/'>
                <Button variant="danger">Home</Button>
            </Link>
            <Link className='navButton' to='/characters/page/1'>
                <Button variant="danger">Characters</Button>
            </Link>
            <Link className='navButton' to='/comics/page/1'>
                <Button variant="danger">Comics</Button>
            </Link>
            <Link className='navButton' to='/stories/page/1'>
                <Button variant="danger">Stories</Button>
            </Link>
            <Link className='navButton' to='/characters/history'>
                <Button variant="danger">History</Button>
            </Link>
        </div>
    )
}

export default Home;