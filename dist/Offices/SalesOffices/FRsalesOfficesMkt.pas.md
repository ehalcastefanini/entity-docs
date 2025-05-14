<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRsalesOfficesMkt` Code Unit

## 1. Overview:

### Objective and Problem Solved:
The `FRsalesOfficesMkt` code unit defines a frame (`TFRAMEsalesOfficesMkt`) that provides a grid-based interface for managing sales offices and their associated markets. It allows users to view, add, and delete market data related to sales offices. The main objective is to provide a user-friendly interface for managing this data while ensuring proper validation and configuration of the grid.

### Technologies Used:
- **Delphi VCL Framework**: Used for creating the user interface and handling events.
- **SOAP Services**: Used for interacting with external services (e.g., `CustomerMarketServiceUtils`).
- **Database Components**: Includes `TDataSet` and `TField` for database interaction.
- **cxGrid**: A component from DevExpress for displaying and managing tabular data.
- **Custom Components**: Includes `TcxEditRepositoryButtonItem` for custom button functionality in the grid.

### Form Type:
This code represents a **grid display**.

#### Grid Columns and Their Types:
1. **marketCode**: Custom editor (`cxEDTfindMarket`).
2. **marketDesc**: Standard text field.

#### Grid Actions and Their Effects:
1. **Add**: Adds a new market entry.
2. **Delete**: Deletes the selected market entry.

---

## 2. Functionality Description:

### User/Software Actions:
1. Add a new market entry.
2. Delete an existing market entry.
3. Search for a market using a custom button in the grid.

### Main Components:
- **Grid (`cxGrid`)**: Displays the market data.
- **Custom Button (`cxEDTfindMarket`)**: Allows users to search for markets.
- **Action List (`AvailableActions`)**: Defines the actions available in the grid (e.g., add, delete).

### Pseudo-code for Actions and Events:
- **Add Button Click**:  
  `if add button clicked then execute ACTaddExecute`
- **Grid Value Change**:  
  `if grid value changed then execute cxDBVtableEditValueChanged`
- **Custom Button Click**:  
  `if custom button clicked then execute m_FindMarket`

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The frame is initialized in the `Create` constructor.
   - Grid settings are configured (e.g., hidden fields, key fields, custom editors).
   - Available actions (`add`, `delete`) are defined.
   - Event handlers are assigned (e.g., `OnSetAccessMode`, `OnButtonClick`).

2. **User Interactions**:
   - Users can add or delete entries using the grid.
   - Users can search for markets using the custom button.

### Functions and File Locations:
1. **`Create`** (Initialization):  
   File: `FRsalesOfficesMkt.pas`  
   Function: `TFRAMEsalesOfficesMkt.Create`
2. **`ACTaddExecute`** (Add Action):  
   File: `FRsalesOfficesMkt.pas`  
   Function: `ACTaddExecute`
3. **`m_FindMarket`** (Search Market):  
   File: `FRsalesOfficesMkt.pas`  
   Function: `m_FindMarket`

### Data Required:
- **Market Code**: Unique identifier for the market.
- **Market Description**: Description of the market.

---

## 4. Business Rules:

### Actions and Preconditions:
1. **Add Action**:
   - Preconditions: None.
   - Action: Adds a new market entry.
2. **Delete Action**:
   - Preconditions: A market entry must be selected.
   - Action: Deletes the selected market entry.

### Available Filters:
- No explicit filters are defined in the code.

### Error Messages:
- Not explicitly defined in the code.

### Default Field Values:
- Not explicitly defined in the code.

### Field Validation and Conditions:
- **marketCode**: Custom editor (`cxEDTfindMarket`) is used for validation.
- **marketDesc**: No specific validation is defined.

---

## 5. Main Functions:

1. **`Create`**: Initializes the frame and configures the grid settings.
2. **`ACTaddExecute`**: Handles the "Add" action.
3. **`m_FindMarket`**: Handles the custom button click for searching markets.
4. **`m_OnSetAccessMode`**: Configures access mode and grid column widths.
5. **`VerifyDataBeforeSave`**: Ensures data is valid before saving (not fully implemented in the provided code).

---

## 6. API Service Consumption:

- **Service Name**: `CustomerMarketServiceUtils`
- **Purpose**: Interacts with external services for market-related operations.
- **Error Handling**: Not explicitly defined in the code.

---

## 7. Conditional Fields (Form Logic):

- **Custom Button (`cxEDTfindMarket`)**: Visible and functional only when the grid is active.

---

## 8. Dependencies:

### External Libraries:
1. **DevExpress Components**: Used for grid and editor functionalities.
2. **SOAPHTTPClient**: Used for SOAP service interactions.

### Custom Components:
1. **`TcxEditRepositoryButtonItem`**: Custom button for searching markets.

---

## 9. Fields and Validations Listing:

1. **marketCode**:  
   - Type: String  
   - Editor: Custom (`cxEDTfindMarket`)  
   - Validation: Not explicitly defined.
2. **marketDesc**:  
   - Type: String  
   - Validation: Not explicitly defined.

---

## 10. Examples and Diagrams:

### Flowchart:
```plaintext
[Start] --> [Initialize Frame] --> [Configure Grid] --> [User Interactions]
    --> [Add/Delete/Search Actions] --> [End]
```

### Sequence Diagram:
```plaintext
User --> Grid: Add/Delete/Search
Grid --> Frame: Trigger Event
Frame --> SOAP Service: Fetch/Save Data
SOAP Service --> Frame: Return Response
Frame --> Grid: Update Display
```

### Code Snippets:
```delphi
procedure TFRAMEsalesOfficesMkt.ACTaddExecute(Sender: TObject);
begin
  // Code to add a new market entry
end;
```

### Screenshots:
Not applicable (no DFM file provided).

---

## 11. Important Comments in the Code:

1. **Grid Configuration**:
   - Hidden fields: `HIDE_ALL_FIELDS`
   - Key fields: `salesdir`
   - Custom editor: `marketCode` uses `cxEDTfindMarket`.

2. **Available Actions**:
   - Defined as `add;delete`.

---

## 12. Conclusion:

The `FRsalesOfficesMkt` code unit provides a robust framework for managing sales offices and their markets. It leverages DevExpress components for a feature-rich grid interface and integrates with SOAP services for data operations. However, the code lacks explicit error handling and field validation, which could be improved.

---

## 13. Short Summary:

The `FRsalesOfficesMkt` unit implements a grid-based interface for managing sales office markets, supporting add, delete, and search functionalities. It uses DevExpress components and SOAP services for data operations, with configurable grid settings and custom editors.#### **FRsalesOfficesMkt.pas**

```
unit FRsalesOfficesMkt;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, sFrameAdapter, ImgList, ActnList, ExtCtrls, Rio,
  SOAPHTTPClient, DBClient, kneFRGridManager, StdCtrls, Buttons, sBitBtn,
  sPanel, cxGridLevel, cxClasses, cxControls, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid;

type
  TFRAMEsalesOfficesMkt = class(TFRAMEBaseGridEditSOA)
    cxEDTfindMarket: TcxEditRepositoryButtonItem;
    procedure ACTaddExecute(Sender: TObject);
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);

  private
    { Private declarations }
    procedure m_OnSetAccessMode(Sender: TObject; var pv_value: Boolean);
    function VerifyDataBeforeSave: Boolean;
    procedure m_ConfigFields(const Dataset: TDataSet;
      const Datapacket: string);
    function SetFieldRequiredState(const pv_Field: TField;
      pv_State: Boolean): Boolean;
    procedure m_FindMarket(Sender: TObject; AButtonIndex: Integer);
    procedure m_FindByCodeMarket(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  end;

var
  FRAMEsalesOfficesMkt: TFRAMEsalesOfficesMkt;

const
  gc_ColsWith =  '100;150;';

implementation

uses
  kneTypes, kneFGControlsUtils, Global, kneUtils, kneFindDialog,
  kneDialogFactory, kneFGFindUtils,
  //Frames, Forms
  MsalesOffices,
  // ServiceUtils
  CustomerMarketServiceUtils;

{$R *.dfm}

constructor TFRAMEsalesOfficesMkt.Create(AOwner: TComponent);
var
  lv_form:  TCustomForm;
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'officeCode';
  DataPacketName := 'OfficeMkt';        // o nome do detail no datapacket(metadata) � sempre no singular
  PropertyName := 'markets';                  // nome do campo da metadata que vai conter os details
  FrameType := frtDetail;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  AvailableActions := 'add;delete';

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin
    // Campos Read-Only ........................................................
    // Campos Hidden ...........................................................
    HiddenFields.Add('HIDE_ALL_FIELDS');
    // Ordem Campos ............................................................
    OrderFields.Add('marketCode');
    OrderFields.Add('marketDesc');
    // Key Fields ..............................................................
    KeyFields:= 'salesdir';
    // Custom Editors ..........................................................
    AddCustomField('marketCode','cxEDTfindMarket');
    UseColsBestFit := False;
  end; //with

  OnSetAccessMode := m_OnSetAccessMode;
  cxEDTfindMarket.Properties.OnButtonClick := m_FindMarket;

  lv_form := TkneFGControlsUtils.fg_GetOwnerForm(Self);
end;

procedure TFRAMEsalesOfficesMkt.m_OnSetAccessMode(Sender: TObject;var pv_value: Boolean);
begin
  inherited;

  SetColsWidthInGrid(gc_ColsWith, cxDBVtable);

  if (not assigned(CDStable)) or (not CDStable.Active) then 
```

#### **FRsalesOfficesMkt.dfm**

```
inherited FRAMEsalesOfficesMkt: TFRAMEsalesOfficesMkt
  inherited cxDBG: TcxGrid
    inherited cxDBVtable: TcxGridDBTableView
      OnEditValueChanged = cxDBVtableEditValueChanged
    end
  end
  inherited cxEDTR: TcxEditRepository
    object cxEDTfindMarket: TcxEditRepositoryButtonItem
      Properties.Buttons = <
        item
          Default = True
          Glyph.Data = {
            36030000424D3603000000000000360000002800000010000000100000000100
            18000000000000030000120B0000120B00000000000000000000FF00FF4A667C
            BE9596FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FF6B9CC31E89E84B7AA3C89693FF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF4BB4FE51B5FF
            2089E94B7AA2C69592FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FF51B7FE51B3FF1D87E64E7AA0CA9792FF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            51B7FE4EB2FF1F89E64E7BA2B99497FF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF52B8FE4BB1FF2787D95F6A76FF
            00FFB0857FC09F94C09F96BC988EFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            FF00FFFF00FF55BDFFB5D6EDBF9D92BB9B8CE7DAC2FFFFE3FFFFE5FDFADAD8C3
            B3B58D85FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFCEA795FD
            EEBEFFFFD8FFFFDAFFFFDBFFFFE6FFFFFBEADDDCAE837FFF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFC1A091FBDCA8FEF7D0FFFFDBFFFFE3FFFFF8FFFF
            FDFFFFFDC6A99CFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFC1A091FEE3ACF1
            C491FCF2CAFFFFDDFFFFE4FFFFF7FFFFF7FFFFE9EEE5CBB9948CFF00FFFF00FF
            FF00FFFF00FFFF00FFC2A191FFE6AEEEB581F7DCAEFEFDD8FFFFDFFFFFE3FFFF
            E4FFFFE0F3ECD2BB968EFF00FFFF00FFFF00FFFF00FFFF00FFBC978CFBE7B7F4
            C791F2C994F8E5B9FEFCD8FFFFDDFFFFDCFFFFE0E2D2BAB68E86FF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFD9C3A9FFFEE5F7DCB8F2C994F5D4A5FAE8BDFDF4
            C9FDFBD6B69089FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFB58D85E8
            DEDDFFFEF2F9D8A3F4C48CF9D49FFDEAB8D0B49FB89086FF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFFF00FFAD827FC9AA9EEFE0B7EFDFB2E7CEACB890
            86B89086FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFBA968ABB988CB79188FF00FFFF00FFFF00FFFF00FF}
          Kind = bkGlyph
        end>
    end
  end
end
```
<!-- tabs:end -->

