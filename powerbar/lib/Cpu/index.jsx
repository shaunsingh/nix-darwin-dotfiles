import { container, arrow, content } from './style.jsx';

const render = ({output}) => {
  if (typeof output === 'undefined') return null;
  return (
    <div style={container}>
      <div style={arrow}/>
      <div style={content}>
        <i class="fas fa-microchip"/>&nbsp;{output.loadAverage}
      </div>
    </div>
  )
}

export default render
