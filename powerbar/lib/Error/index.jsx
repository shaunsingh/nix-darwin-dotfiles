import { containerLeft, containerRight, arrowLeft, arrowRight, contentLeft, contentRight } from './style.jsx';

const render = ({msg, side}) => {
  if (typeof msg === 'undefined') return null;
  if (side === 'left' || typeof side === 'undefined') {
    return (
      <div style={containerLeft}>
        <div style={contentLeft}>
          <i class="fas fa-exclamation-triangle"/>&nbsp;{msg}
        </div>
        <div style={arrowLeft}/>
      </div>
    )
  }
  return (
    <div style={containerRight}>
      <div style={arrowRight}/>
      <div style={contentRight}>
        <i class="fas fa-exclamation-triangle"/>&nbsp;{msg}
      </div>
    </div>
  )
}

export default render
