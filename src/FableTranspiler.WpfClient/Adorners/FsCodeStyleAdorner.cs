using FableTranspiler.WpfClient.UserControls;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Documents;
using System.Windows.Media;

namespace FableTranspiler.WpfClient.Adorners
{
    public class FsCodeStyleAdorner : Adorner
    {
        private readonly VisualCollection _visuals;
        private readonly Canvas _canvas;
        private readonly FsCodeStyleSwitcher _switcher;


        public FsCodeStyleAdorner(UIElement adornedElement) : base(adornedElement)
        {
            _visuals = new VisualCollection(this);

            _canvas = new Canvas();

            _switcher = new FsCodeStyleSwitcher();
            Canvas.SetLeft(_switcher, 50);
            Canvas.SetTop(_switcher, 50);


            _canvas.Children.Add(_switcher);

            _visuals.Add(_canvas);

            Hide();
        }


        internal void Hide()
        {
            _canvas.Visibility = Visibility.Collapsed;
        }


        internal void SetPosition( Block block )
        {
            _canvas.Visibility = Visibility.Visible;
            var start = block.ContentStart.GetCharacterRect(LogicalDirection.Backward);
            Canvas.SetLeft(_switcher, start.Left);
            Canvas.SetTop(_switcher, start.Top);
        }

        protected override int VisualChildrenCount
        {
            get {
                return _visuals.Count;
            }
        }


        protected override Visual GetVisualChild(int index)
        {
            return _visuals[index];
        }

        protected override Size MeasureOverride(Size constraint)
        {
            return base.MeasureOverride(constraint);
        }

        protected override Size ArrangeOverride(Size finalSize)
        {
            _canvas.Arrange(new Rect(finalSize));
            return base.ArrangeOverride(finalSize);
        }
    }
}
