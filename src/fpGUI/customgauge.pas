{
  This unit defines a custom Gauge descendant which improves the visibility
  of the state of the test run. Instead of setting the background color, the
  progress bar is painted in the test state color itself.

  Copyright (c) 2011 by Graeme Geldenhuys
    All rights reserved.
}
unit CustomGauge;

{$mode objfpc}{$H+}

interface

uses
  fpg_base, fpg_main, fpg_gauge;

type
  TGUIRunnerGauge = class(TfpgBaseGauge)
  protected
    procedure BarDraw; override;
    procedure BackgroundDraw; override;
  published
    property    Align;
    property    Anchors;
    property    BorderStyle;
    property    Color;
    property    Enabled;
    property    FirstColor;
    property    Hint;
    property    Kind;
    property    MaxValue;
    property    MinValue;
    property    ParentShowHint;
    property    Progress;
    property    SecondColor;
    property    ShowHint;
    property    ShowText;
    property    Visible;
    property    OnShowHint;
  end;


implementation

{ TGUIRunnerGauge }

procedure TGUIRunnerGauge.BarDraw;
var
  BarLength: Longint;
  SavedRect: TfpgRect;
begin
  SavedRect := FClientRect; // save client rect for text !!
  with FClientRect do
  begin
    case FKind of
      gkHorizontalBar:
          begin
            { now paint as normal }
            BarLength := Longint(Trunc( (Width * Percentage) / 100.0 ) );
            if BarLength > 0 then
            begin
              if BarLength > Width then
                BarLength := Width;
              Width := BarLength;
              // left top
              Canvas.SetColor(fpgLighter(Color, 45));
              Canvas.DrawLine(Left, Bottom, Left, Top);  // left
              Canvas.DrawLine(Left, Top, Right, Top);    // top
              // right bottom
              Canvas.SetColor(fpgDarker(Color, 45));
              Canvas.DrawLine(Right, Top, Right, Bottom);   // right
              Canvas.DrawLine(Right, Bottom, Left, Bottom);   // bottom
              // inside gradient fill
              InflateRect(-1, -1);
              Canvas.GradientFill(FClientRect, Color, fpgLighter(Color, 45), gdVertical);
            end;  { if }
            FClientRect := SavedRect;
          end;
      gkVerticalBar:
          begin
            inherited BarDraw;
          end;
    end;  { case }
  end;  { FClientRect }
end;

procedure TGUIRunnerGauge.BackgroundDraw;
begin
  FClientRect.SetRect(0, 0, Width, Height);
  with FClientRect do
  begin
    { Kind specific Bacground }
    case FKind of
      { Currently Text doesn't require additional Bacground }
      { And so horizontal and vertical bar - Unless style requires it}
      gkHorizontalBar:
          begin
            {Client area is Widget area, to start with}
            Canvas.ClearClipRect;
            Canvas.Clear(TfpgColor($c4c4c4));
            { This must be adjusted according the selected style }
            Canvas.SetColor(TfpgColor($999999));
            Canvas.SetLineStyle(1, lsSolid);
            Canvas.DrawRectangle(FClientRect);
            { This must be completed and adjusted with border style }
            InflateRect(-1, -1);
            Canvas.SetLineStyle(1, lsSolid); // just in case background changed that
          end;
      else
          begin
            inherited BackgroundDraw;
          end;
    end;
  end; { with }
end;

end.

