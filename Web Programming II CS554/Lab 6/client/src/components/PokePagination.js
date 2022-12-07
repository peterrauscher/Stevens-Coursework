import { Pagination } from "react-bootstrap";
import { Link } from "react-router-dom";

const PokePagination = (props) => {
  const pageNum = parseInt(props.pagenum);
  const maxPageNum = parseInt(props.maxpagenum);

  const paginationItem = (n, active = false) => {
    if (active)
      return (
        <li className="page-item active">
          <Link className="page-link" to={`/pokemon/page/${n}`}>
            {n}
            <span className="visually-hidden">(current)</span>
          </Link>
        </li>
      );
    else
      return (
        <li className="page-item">
          <Link
            className="page-link"
            role="button"
            tabIndex="0"
            to={`/pokemon/page/${n}`}
          >
            {n}
          </Link>
        </li>
      );
  };

  const nextButton = (n) => {
    return (
      <li className="page-item">
        <Link
          className="page-link"
          role="button"
          tabIndex="0"
          to={`/pokemon/page/${n}`}
        >
          <span aria-hidden="true">›</span>
          <span className="visually-hidden">Next</span>
        </Link>
      </li>
    );
  };

  const prevButton = (n) => {
    return (
      <li className="page-item">
        <Link
          className="page-link"
          role="button"
          tabIndex="0"
          to={`/pokemon/page/${n}`}
        >
          <span aria-hidden="true">‹</span>
          <span className="visually-hidden">Previous</span>
        </Link>
      </li>
    );
  };

  const ellipses = (n) => {
    return (
      <li className="page-item">
        <Link
          className="page-link"
          role="button"
          tabIndex="0"
          to={`/pokemon/page/${n}`}
        >
          <span aria-hidden="true">…</span>
          <span className="visually-hidden">More</span>
        </Link>
      </li>
    );
  };

  const firstHalfPagination = () => {
    if (pageNum === 0) return <></>;
    else if (pageNum === 1)
      return (
        <>
          {prevButton(0)}
          {paginationItem(0)}
        </>
      );
    else if (pageNum === 2)
      return (
        <>
          {prevButton(1)}
          {paginationItem(0)}
          {paginationItem(1)}
        </>
      );
    else if (pageNum === 3)
      return (
        <>
          {prevButton(2)}
          {paginationItem(0)}
          {paginationItem(1)}
          {paginationItem(2)}
        </>
      );
    else if (pageNum > 3)
      return (
        <>
          {prevButton(pageNum - 1)}
          {paginationItem(0)}
          {ellipses(pageNum - 3)}
          {paginationItem(pageNum - 2)}
          {paginationItem(pageNum - 1)}
        </>
      );
  };

  const secondHalfPagination = () => {
    if (pageNum === maxPageNum) return <></>;
    else if (pageNum === maxPageNum - 1)
      return (
        <>
          {paginationItem(pageNum + 1)}
          {nextButton(pageNum + 1)}
        </>
      );
    else if (pageNum === maxPageNum - 2)
      return (
        <>
          {paginationItem(pageNum + 1)}
          {paginationItem(pageNum + 2)}
          {nextButton(pageNum + 1)}
        </>
      );
    else if (pageNum === maxPageNum - 3)
      return (
        <>
          {paginationItem(pageNum + 1)}
          {paginationItem(pageNum + 2)}
          {paginationItem(maxPageNum)}
          {nextButton(pageNum + 1)}
        </>
      );
    else if (pageNum < maxPageNum - 3)
      return (
        <>
          {paginationItem(pageNum + 1)}
          {paginationItem(pageNum + 2)}
          {ellipses(pageNum + 3)}
          {paginationItem(maxPageNum)}
          {nextButton(pageNum + 1)}
        </>
      );
  };

  return (
    <Pagination className="d-flex justify-content-center">
      {firstHalfPagination()}
      {paginationItem(pageNum, true)}
      {secondHalfPagination()}
    </Pagination>
  );
};

export default PokePagination;
