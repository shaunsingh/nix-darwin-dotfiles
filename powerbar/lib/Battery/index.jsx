import { container, arrow, content } from './style.jsx';

const displayIcon = (batteryPercentage, isCharging) => {
  if (isCharging === true) {
    return 'fa-bolt';
  } else if (batteryPercentage < 20) {
    return 'fa-battery-empty';
  } else if (batteryPercentage < 40) {
    return 'fa-battery-quarter';
  } else if (batteryPercentage < 60) {
    return 'fa-battery-half';
  } else if (batteryPercentage < 80) {
    return 'fa-battery-three-quarters';
  } else {
    return 'fa-battery-full';
  }
}

const updateStyling = (batteryPercentage, isCharging) => {
  let contentStyle = JSON.parse(JSON.stringify(content));
  let arrowStyle = JSON.parse(JSON.stringify(arrow));
  if (isCharging === true) {
    contentStyle.background = 'rgba(163, 189, 140, 1)';
    contentStyle.color = 'rgba(76, 86, 106, 1)';
    arrowStyle.borderRight = '10px solid rgba(163, 189, 140, 1)';
  } else if (batteryPercentage < 60) {
    contentStyle.background = 'rgba(235, 203, 139, 1)';
    contentStyle.color = 'rgba(76, 86, 106, 1)';
    arrowStyle.borderRight = '10px solid rgba(235, 203, 139, 1)';
  } else if (batteryPercentage < 40) {
    contentStyle.background = 'rgba(208, 135, 113, 1)';
    contentStyle.color = 'rgba(76, 86, 106, 1)';
    arrowStyle.borderRight = '10px solid rgba(208, 135, 113, 1)';
  } else if (batteryPercentage < 20) {
    contentStyle.background = 'rgba(191, 97, 106, 1)';
    contentStyle.color = 'rgba(76, 86, 106, 1)';
    arrowStyle.borderRight = '10px solid rgba(191, 97, 106, 1)';
  }
  return { contentStyle, arrowStyle };
}

const render = ({output}) => {
  if (typeof output === 'undefined') return null;
  const batteryIcon = displayIcon(output.percentage, output.charging);
  const iconClasses = `fas ${batteryIcon}`;
  const { contentStyle, arrowStyle } = updateStyling(output.percentage, output.charging);
  return (
    <div style={container}>
      <div style={arrowStyle}/>
      <div style={contentStyle}>
        <i class={iconClasses}/>&nbsp;{output.percentage}%
      </div>
    </div>
  )
}

export default render
