import { container, arrow, content } from './style.jsx';

const render = ({output}) => {
  if (typeof output === 'undefined') return null;
	const usedMemory = output.total - output.free;
	const memoryConsumption = Math.round(output.free / output.total * 100.0);
  return (
    <div style={container}>
      <div style={arrow}/>
      <div style={content}>
        <i class="fas fa-memory"/>&nbsp;{memoryConsumption}%
      </div>
    </div>
  )
}

export default render
