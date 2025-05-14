<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRsalesDirMkt` Code Unit

## 1. Overview:

### Objective:
The `FRsalesDirMkt` code unit defines a Delphi frame (`TFRAMEsalesDirMkt`) that extends a base grid-editing frame (`TFRAMEBaseGridEditSOA`). Its primary purpose is to manage and display a grid interface for sales directory and market data. It provides functionalities such as adding, deleting, and editing market-related data, with specific configurations for grid behavior and field properties.

### Technologies Used:
- **Delphi Framework**: For creating the frame and managing UI components.
- **DevExpress cxGrid**: For grid display and data manipulation.
- **SOAP Services**: For interacting with external services (e.g., `CustomerMarketServiceUtils`).
- **Database Components**: For managing and interacting with datasets.

### Form Type:
This is a **grid display** form.

#### Grid Columns and Their Types:
1. **marketCode**: String (Custom editor: `cxEDTfindMarket`).
2. **marketDesc**: String.

#### Grid Actions and Their Effects:
1. **Add**: Adds a new market entry.
2. **Delete**: Deletes the selected market entry.

---

## 2. Functionality Description:

### User/Software Actions:
1. Add a new market entry.
2. Delete an existing market entry.
3. Edit values in the grid.
4. Search for a market using a custom button editor.

### Main Components:
- **Grid (`cxGrid`)**: Displays market data.
- **Custom Editor (`cxEDTfindMarket`)**: Allows searching for markets.
- **Action List**: Defines actions like "Add" and "Delete."

### Pseudo-code for Actions and Events:
- **Add Button Click**: `if add button clicked then execute add action`.
- **Delete Button Click**: `if delete button clicked then execute delete action`.
- **Grid Value Change**: `if grid cell value changed then validate and update data`.
- **Search Button Click**: `if search button clicked then open market search dialog`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The frame is created and configured in the `Create` constructor.
   - Grid settings, such as hidden fields, key fields, and custom editors, are applied.
   - Available actions (`add`, `delete`) are defined.

2. **User Interactions**:
   - Clicking the "Add" button triggers the `ACTaddExecute` procedure.
   - Editing a grid cell triggers the `cxDBVtableEditValueChanged` event.
   - Clicking the search button in the custom editor triggers the `m_FindMarket` procedure.

### Data Requirements:
- Users must provide valid market codes and descriptions when adding or editing entries.

---

## 4. Business Rules:

### Actions and Preconditions:
1. **Add Action**:
   - Preconditions: None.
   - Action: Adds a new market entry to the grid.

2. **Delete Action**:
   - Preconditions: A row must be selected in the grid.
   - Action: Deletes the selected market entry.

3. **Edit Action**:
   - Preconditions: The grid must be in edit mode.
   - Action: Updates the value of the edited field.

### Available Filters:
- No explicit filters are defined in the code.

### Error Messages:
- No explicit error messages are defined in the code.

### Default Field Values:
- No default values are explicitly defined in the code.

### Field Validation and Conditions:
- **marketCode**: Custom editor (`cxEDTfindMarket`) is used for validation.
- **marketDesc**: No specific validation is defined.

---

## 5. Main Functions:

1. **`Create` Constructor**:
   - Configures the frame, grid settings, and available actions.
   - Sets up event handlers for custom editors and access mode.

2. **`ACTaddExecute`**:
   - Handles the "Add" action.

3. **`cxDBVtableEditValueChanged`**:
   - Handles changes to grid cell values.

4. **`m_FindMarket`**:
   - Opens a search dialog for markets.

5. **`VerifyDataBeforeSave`**:
   - Validates data before saving (implementation not provided in the snippet).

---

## 6. API Service Consumption:

- **Service Name**: `CustomerMarketServiceUtils`.
- **Purpose**: Interacts with external services for market-related operations.
- **Details**: Specific API calls are not defined in the provided code.

---

## 7. Conditional Fields (Form Logic):

- **Custom Editor (`cxEDTfindMarket`)**:
  - Appears as a button editor in the grid.
  - Triggers a search dialog when clicked.

---

## 8. Dependencies:

### External Libraries:
1. **DevExpress cxGrid**: For grid display and data manipulation.
2. **SOAPHTTPClient**: For SOAP service interactions.

### Custom Components:
1. **`TFRAMEBaseGridEditSOA`**: Base frame for grid editing.
2. **`CustomerMarketServiceUtils`**: Utility for market-related SOAP services.

---

## 9. Fields and Validations Listing:

1. **marketCode**:
   - Type: String.
   - Custom Editor: `cxEDTfindMarket`.
   - Validation: Not explicitly defined in the code.

2. **marketDesc**:
   - Type: String.
   - Validation: Not explicitly defined in the code.

---

## 10. Examples and Diagrams:

### Flowchart:
```plaintext
[Start] --> [Initialize Frame] --> [Load Grid Data] --> [User Interaction]
    --> [Add/Delete/Edit Actions] --> [Save Changes] --> [End]
```

### Sequence Diagram:
```plaintext
User --> Frame: Click Add/Delete/Edit
Frame --> Grid: Update Data
Grid --> SOAP Service: Send/Receive Data
SOAP Service --> Frame: Return Response
```

### Code Snippets:
```delphi
procedure TFRAMEsalesDirMkt.ACTaddExecute(Sender: TObject);
begin
  // Add action logic here
end;
```

### Screenshots:
Not applicable (no DFM file provided).

---

## 11. Important Comments in the Code:

1. **Grid Settings**:
   - Hidden fields: `HIDE_ALL_FIELDS`.
   - Key fields: `salesdir`.
   - Custom editor: `marketCode` uses `cxEDTfindMarket`.

2. **Available Actions**:
   - Defined as `add;delete`.

3. **Event Handlers**:
   - `OnSetAccessMode`: Configures access mode for the frame.

---

## 12. Conclusion:

The `FRsalesDirMkt` code unit provides a robust framework for managing sales directory and market data through a grid interface. It leverages DevExpress components for grid functionality and integrates with SOAP services for external data operations. However, the code lacks explicit error handling, field validation, and detailed API interaction definitions.

---

## 13. Short Summary:

The `FRsalesDirMkt` unit defines a grid-based interface for managing sales directory and market data, supporting add, delete, and edit actions. It integrates with SOAP services and uses DevExpress components for enhanced UI functionality.#### **FRsalesDirMkt.pas**

```
unit FRsalesDirMkt;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, sFrameAdapter, ImgList, ActnList, ExtCtrls, Rio,
  SOAPHTTPClient, DBClient, kneFRGridManager, StdCtrls, Buttons, sBitBtn,
  sPanel, cxGridLevel, cxClasses, cxControls, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid,
  kneFREditSOA;

type
  TFRAMEsalesDirMkt = class(TFRAMEBaseGridEditSOA)
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
  FRAMEsalesDirMkt: TFRAMEsalesDirMkt;

const
  gc_ColsWith =  '100;150;';

implementation

uses
  kneTypes, kneFGControlsUtils, Global, kneUtils, kneFindDialog,
  kneDialogFactory, kneFGFindUtils,
  //Frames, Forms
  MsalesDir,
  // ServiceUtils
  CustomerMarketServiceUtils;

{$R *.dfm}

{ TFRAMEsalesDirMkt }

constructor TFRAMEsalesDirMkt.Create(AOwner: TComponent);
var
  lv_form:  TCustomForm;
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'salesdir';
  DataPacketName := 'SalesdirMkt';        // o nome do detail no datapacket(metadata) � sempre no singular
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

procedure TFRAMEsalesDirMkt.m_OnSetAccessMode(Sender: TObject;var pv_value: Boolean);
begin
  inherited;

```

#### **FRsalesDirMkt.dfm**

```
inherited FRAMEsalesDirMkt: TFRAMEsalesDirMkt
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
      Properties.CharCase = ecUpperCase
      Properties.ClickKey = 114
    end
  end
end
```
<!-- tabs:end -->

