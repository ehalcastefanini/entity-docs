<!-- tabs:start -->

#### **Documentation**

# Documentation for `MconsMarket` Code Unit

## 1. Overview:

### Objective:
The `MconsMarket` code unit is designed to manage the "Consignee Market Management" form. It provides a user interface for managing consignee market data, including displaying, editing, and deleting records. The form integrates with external data sources and frames to fetch and display data dynamically.

### Technologies Used:
- **Delphi (Object Pascal):** The code is written in Delphi, utilizing its VCL (Visual Component Library) for GUI development.
- **Custom Components:** Includes custom components like `kneCBedit`, `kneFRGridEditSOA`, `kneFREditSOA`, and others for specialized functionalities.
- **Third-party Libraries:** Includes `sPanel`, `sBitBtn`, and `acCoolBar` for enhanced UI elements.

### Form Type:
This is a **form** with the following elements:
- **Form Elements:**
  - `PNLconsMkt` (Panel): Container for the main content.
  - `FRAMEconsMarket1` (Frame): Displays market-related data.
  - `FRAMEextShipDelConsMkt1` (Frame): Displays extended shipping and delivery data.
- **Form Actions:**
  - `m_getData`: Fetches and initializes data for the form.
  - `m_CreateFormEdit`: Creates and initializes the form instance.

---

## 2. Functionality Description:

### User/Software Actions:
- **View Data:** Users can view consignee market data fetched from external sources.
- **Edit Data:** Users can edit the data displayed in the form.
- **Delete Data:** Users can delete records using the delete button.

### Main Components:
1. **`PNLconsMkt`:** A panel that acts as the container for the form's main content.
2. **`FRAMEconsMarket1`:** A frame that handles the display and interaction with market data.
3. **`FRAMEextShipDelConsMkt1`:** A frame for extended shipping and delivery data.
4. **`m_getData`:** A method to fetch and initialize data for the form.

### Pseudo-code for Actions and Events:
- **Form Initialization:**
  - `if form is created then initialize components and fetch data`.
- **Data Fetching:**
  - `if m_getData is called then fetch data from the master frame and set parameters`.
- **Button Click (Delete):**
  - `if delete button clicked then delete selected record`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization:**
   - The form is created using the `m_CreateFormEdit` method.
   - Components like `PNLconsMkt`, `FRAMEconsMarket1`, and `FRAMEextShipDelConsMkt1` are initialized.
2. **Data Fetching:**
   - The `m_getData` method is called to fetch data from the master frame (`TFRAMEBaseEditSOA`).
   - Standard service parameters are set (e.g., `ShowInactives`).
   - Data is linked to the `FRAMEextShipDelConsMkt1` frame.
3. **User Interaction:**
   - Users can interact with the form to view, edit, or delete data.

### Required Data:
- No specific user input is required for initialization.
- Users interact with the form to view or modify data.

---

## 4. Business Rules:

### Actions and Preconditions:
- **Delete Action:**
  - Preconditions: A record must be selected.
  - Action: Deletes the selected record.
- **Data Fetching:**
  - Preconditions: The form must be initialized.
  - Action: Fetches data from the master frame.

### Available Filters:
- **Show Inactives:** A parameter to include inactive records in the data.

### Error Messages:
- No explicit error messages are defined in the code.

### Default Field Values:
- **ShowInactives:** Default is `True`.

### Field Validation and Conditions:
- No explicit field validations are defined in the code.

---

## 5. Main Functions:

1. **`m_CreateFormEdit`:**
   - Creates and initializes the form instance.
   - Business Logic: Ensures the form is properly instantiated and ready for use.

2. **`m_getData`:**
   - Fetches data from the master frame and sets service parameters.
   - Business Logic: Optimizes resource usage and links data to the appropriate frame.

---

## 6. API Service Consumption:

- **Service Name:** Not explicitly defined in the code.
- **Endpoint:** Not explicitly defined in the code.
- **Data Sent:** Not explicitly defined in the code.
- **Data Received:** Not explicitly defined in the code.
- **Purpose:** Fetch and display consignee market data.
- **Error Handling:** Not explicitly defined in the code.

---

## 7. Conditional Fields (Form Logic):

- No conditional fields are explicitly defined in the code.

---

## 8. Dependencies:

### External Libraries:
- **`kneCBedit`, `kneFRGridEditSOA`, `kneFREditSOA`:** Custom components for data editing and grid display.
- **`sPanel`, `sBitBtn`, `acCoolBar`:** Third-party UI components for enhanced visuals.

### Custom Components:
- **`TFRAMEconsMarket`:** Custom frame for market data.
- **`TFRAMEextShipDelConsMkt`:** Custom frame for extended shipping and delivery data.

---

## 9. Fields and Validations Listing:

- **Fields:**
  - `ShowInactives` (type: boolean, default: True).
- **Mapping:**
  - No explicit mapping is defined in the code.

---

## 10. Examples and Diagrams:

### Flowchart:
```plaintext
[Start] --> [Create Form] --> [Initialize Components] --> [Fetch Data] --> [Display Data] --> [User Interaction]
```

### Sequence Diagram:
```plaintext
User --> Form: Open Form
Form --> Master Frame: Fetch Data
Master Frame --> Form: Return Data
User --> Form: Interact with Data
```

### Code Snippets:
```pascal
// Create and initialize the form
var
  Form: TFORMMconsMarket;
begin
  Form := TFORMMconsMarket.m_CreateFormEdit(Application);
  Form.Show;
end;
```

### Screenshots:
Not applicable (no DFM file provided).

---

## 11. Important Comments in the Code:

- **Optimization of Resources:**
  - `lv_MasterFrame := TFRAMEBaseEditSOA(kneUtils.TkneGeneric.fg_GetMasterFrame(Self));`
  - Ensures efficient resource usage by fetching the master frame dynamically.

- **Service Parameters:**
  - `lv_MasterFrame.ServiceParams.ShowInactives := True;`
  - Sets default parameters for data fetching.

---

## 12. Conclusion:

The `MconsMarket` code unit provides a robust framework for managing consignee market data. It integrates custom components and frames to fetch and display data dynamically. However, the code lacks explicit error handling, field validations, and detailed API integration, which could be improved for better functionality.

---

## 13. Short Summary:

The `MconsMarket` code unit manages a form for consignee market data, integrating custom frames and components for dynamic data fetching and display. It supports viewing, editing, and deleting records but lacks explicit error handling and validations.#### **MConsMarket.pas**

```
unit MconsMarket;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneCBedit, StdCtrls, Buttons, ExtCtrls, kneFRGridEditSOA,
  kneFREditSOA, ImgList, knePrivileges, sSpeedButton,
  sBitBtn, ToolWin, ComCtrls, acCoolBar, sPanel, kneEnterAsTab,
  sPageControl, kneFRCtrlEditSOA, FRconsMarket, ActnList, 
  FRextShipDelConsMkt;

type
  TFORMMconsMarket = class(TFORMkneBaseEdit)
    PNLconsMkt: TsPanel;
    FRAMEconsMarket1: TFRAMEconsMarket;
    FRAMEextShipDelConsMkt1: TFRAMEextShipDelConsMkt;
  private
    { Private declarations }
  protected
    procedure m_getData; override;
  public
    { Public declarations }
    class function m_CreateFormEdit(const AOwner: TComponent): TFORMkneBaseEdit; override;
  end;

var
  FORMMconsMarket: TFORMMconsMarket;

implementation

uses kneUtils;

{$R *.dfm}

{ TFORMMConsMarket }

class function TFORMMconsMarket.m_CreateFormEdit(const AOwner: TComponent): TFORMkneBaseEdit;
begin
  // Substituir pelo nome do form
  Result := TFORMMConsMarket.Create(Application);
end;

procedure TFORMMconsMarket.m_getData;
var
  lv_MasterFrame: TFRAMEBaseEditSOA;
begin
  Screen.Cursor := crHourGlass;
  // optimiza��o de recursos
  lv_MasterFrame := TFRAMEBaseEditSOA(kneUtils.TkneGeneric.fg_GetMasterFrame(Self));

  // parametros standard de servi�os
  lv_MasterFrame.ServiceParams.ShowInactives := True;
//  lv_MasterFrame.ServiceParams.MaxRecords := 0;
//  lv_MasterFrame.ServiceParams.Criteria := '';


  FRAMEextShipDelConsMkt1.MasterSource := lv_MasterFrame.DStable;  // [#21211]

  inherited m_getData;
end;

end.
```

#### **MConsMarket.dfm**

```
inherited FORMMconsMarket: TFORMMconsMarket
  Left = 250
  Top = 215
  Width = 676
  Height = 448
  Caption = 'Consignee Market Management'
  Font.Charset = ANSI_CHARSET
  Font.Name = 'Verdana'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited PNLtoolbar: TsPanel
    Width = 660
    inherited CLBactions: TsCoolBar
      Width = 660
      Bands = <
        item
          Control = PNbotoes
          ImageIndex = -1
          MinHeight = 41
          Width = 656
        end>
      inherited PNbotoes: TsPanel
        Width = 643
        inherited PNLdelete: TsPanel
          inherited BTNDelete: TsBitBtn
            Glyph.Data = {
              36090000424D3609000000000000360000002800000018000000180000000100
              2000000000000009000000000000000000000000000000000000FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF007B3921006331
              310063313100633131006331310063313100FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF006331310031319C0031319C003131
              CE0031319C003131CE0031319C0031319C003131630031313100FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF009C5A390031319C003131CE003131CE003131CE00315A
              E700315AE700315AE7003131CE003131CE003131CE0031319C0031319C003131
              3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00633163003131CE003131CE00315AE700315AE700315AE700315A
              E700315AE700315AE700315AE700315AE700315AE700315AE7003131CE003131
              9C0031313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF009C6363003131CE00315AE700315AE700315AE700315AE700315AE700315A
              E700315AE700315AE700315AE700315AE700315AE700315AE700315AE7003163
              CE003131CE0031313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
              63003131CE00315AE700315AE700315AE700315AE7003163FF00315AE700315A
              E700315AE700315AE700315AE700315AE700315AE700315AE700315AE700315A
              E7003131CE003131CE0031313100FF00FF00FF00FF00FF00FF00FF00FF003131
              9C00315AE700315AE700315AE700315AE700315AE700315AE700315AE700315A
              E700315AE700315AE700315AE700315AE7003163FF00315AE700315AE7003163
              FF00315AE7003131CE0031319C0063313100FF00FF00FF00FF00B5735A00315A
              E700315AE700315AE700315AE700315AE700315AE700315AE700315AE700315A
              E700315AE7003163FF00315AE700315AE700315AE700315AE700315AE700315A
              E700315AE700315AE7003131CE0031313100FF00FF00FF00FF0063319C00315A
              E700315AE700315AE700315AE700315AE700315AE700315AE700315AE7003163
              FF00315AE700315AE700315AE700315AE700315AE700315AE700315AE700315A
              E700315AE700315AE7003131CE0031319C007B392100FF00FF003163CE00315A
              E700315AE700315AE700315AE700315AE7003163FF00315AE700315AE700315A
              E700315AE700315AE700315AE700315AE700315AE700315AE700315AE700315A
              E700315AE700315AE700315AE7003131CE0063310000FF00FF00315AE700315A
              E7003163FF003163FF00CEEFF700CECEFF00CECEFF00CECEFF00CECEFF00CECE
              FF00CECEFF00CECEFF00CECEFF00CECEFF00CECEFF00CECEFF00CECEFF00CECE
              FF00315AE700315AE700315AE7003131CE0063313100FF00FF00315AE700315A
              E700315AE700315AE700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
              FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
              FF00315AE700315AE700315AE7003163CE0063313100FF00FF00315AE700315A
              E700319CFF003163FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
              FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
              FF00315AE700315AE700315AE7003131CE0063313100FF00FF00315AE700315A
              E7006363FF006363CE00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
              FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
              FF00315AE700315AE700315AE700315AE7007B392100FF00FF00315AE700315A
              E700639CFF00639CFF00639CFF00639CFF00639CCE00639CFF006363FF00639C
              FF006363FF003163FF003163CE003163FF003163CE00315AE700315AE700315A
              E700315AE700315AE700315AE7003131CE0094422900FF00FF0063639C00315A
              E700639CFF00639CFF00639CCE00639CFF00639CFF00639CFF00639CCE00639C
              FF00639CCE00639CFF006363FF003163FF003163FF003163FF00315AE7003163
              FF00315AE700315AE700315AE70031319C009C5A3900FF00FF00CE636300315A
              E7006363FF00639CFF00639CFF00639CFF009C9CFF00639CFF00639CFF00639C
              FF00639CFF006363FF00639CCE006363FF00319CCE003163FF00315AE700315A
              E700315AE700315AE700315AE70063313100FF00FF00FF00FF00FF00FF006363
              CE00315AE700639CFF009C9CFF00A5B5F700639CFF009C9CFF00639CFF009C9C
              FF00639CFF00639CCE00639CFF006363FF003163FF006363FF003163FF00315A
              E700315AE700315AE7003131CE009C5A3900FF00FF00FF00FF00FF00FF00CE63
              6300315AE7003163FF00A5B5F700A5B5F7009CCEFF00A5B5F7009CCEFF00639C
              FF00639CFF00639CFF00639CFF006363FF00639CCE003163FF003163CE003163
              FF00315AE700315AE7009C5A3900FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF009C636300315AE700315AE700A5B5F700CECEFF00A5B5F700A5B5F700A5B5
              F7009C9CFF00639CFF00639CCE00639CFF003163FF006363FF003163FF00315A
              E700315AE70063316300FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF009C636300315AE700315AE700639CFF009CCEFF00A5B5F700A5B5
              F700A5B5F700639CFF00639CFF00639CFF00639CFF00315AE700315AE700315A
              E7009C636300FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00CE6363006363CE00315AE700315AE7006363FF00639C
              FF00639CFF00639CFF006363FF00315AE700315AE700315AE7006363CE009C5A
              3900FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63630063639C00315AE700315A
              E700315AE700315AE700315AE700315AE70063639C009C636300FF00FF00FF00
```
<!-- tabs:end -->

