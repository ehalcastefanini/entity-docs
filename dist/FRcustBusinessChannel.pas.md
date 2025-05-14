<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRcustBusinessChannel`

## 1. Overview:

### Objective:
The `FRcustBusinessChannel` code snippet defines a Delphi frame (`TFRAMEcustBusinessChannel`) that extends a base grid-editing frame (`TFRAMEBaseGridEditSOA`). Its primary purpose is to manage and display customer business channels in a grid format. The frame provides functionalities such as displaying, organizing, and restricting editing of specific fields in the grid. It also includes a mechanism to check if a specific portal channel is activated.

### Technologies Used:
- **Delphi**: Object Pascal programming language for Windows application development.
- **VCL Components**: Includes `cxGrid`, `cxEditRepository`, and other UI components for grid and form management.
- **SOAP**: Used for service-oriented architecture (SOA) integration.
- **Database Components**: Includes `DBClient` for database interaction.

### Form Type:
This is a **grid display**. 

#### Grid Columns and Their Types:
1. **channelCd**: String (Read-only).
2. **chDescrip**: String (Read-only).
3. **checked**: Boolean (Checkbox).

#### Grid Actions and Their Effects:
- **Read-Only Fields**: Prevents editing of specific fields (`channelCd` and `chDescrip`).
- **Hidden Fields**: All fields are hidden by default unless explicitly defined.
- **Custom Editors**: Adds a checkbox editor for the `checked` field.

---

## 2. Functionality Description:

### User/Software Actions:
1. **View Customer Business Channels**: Displays a grid with customer business channel data.
2. **Check Portal Activation**: Determines if a specific portal channel is activated based on the `checked` field.
3. **Restrict Editing**: Prevents editing of the `channelCd` field.

### Main Components:
- **Grid Settings**: Configures the grid's behavior, including read-only fields, hidden fields, and custom editors.
- **Action Panel**: Disabled by default (`ShowActionPanel = False`).
- **Portal Activation Check**: Implements logic to verify if a portal channel is activated.

### Pseudo-Code for Actions and Events:
- **OnCreate**: `if frame created then initialize grid settings and properties`.
- **SetKeyEditing**: `if EditKey is true then allow editing else disable editing for channelCd`.
- **m_IsPortalActivated**: `if channelCd = 'P' and checked = 1 then return true else return false`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The frame is created, and grid settings are configured.
   - Read-only fields, hidden fields, and custom editors are defined.
   - The action panel is hidden, and no actions are available.

2. **User Interaction**:
   - Users view the grid displaying customer business channels.
   - The `checked` field can be toggled using a checkbox.

3. **Functions**:
   - **`Create`** (File: `FRcustBusinessChannel.pas`): Initializes the frame and grid settings.
   - **`SetKeyEditing`** (File: `FRcustBusinessChannel.pas`): Disables editing for the `channelCd` field.
   - **`m_IsPortalActivated`** (File: `FRcustBusinessChannel.pas`): Checks if the portal channel is activated.

### Required Data:
- **Grid Data**: Customer business channel data, including `channelCd`, `chDescrip`, and `checked`.

---

## 4. Business Rules:

### Actions and Preconditions:
- **Grid Display**: Requires customer business channel data to populate the grid.
- **Portal Activation Check**: Requires the `channelCd` field to contain 'P' and the `checked` field to be set to 1.

### Available Filters:
- No filters are explicitly defined in the code.

### Error Messages:
- No error messages are explicitly defined in the code.

### Default Field Values:
- **checked**: Default value is unchecked (`0`).

### Field Validation and Conditions:
- **channelCd**: Read-only, cannot be edited.
- **checked**: Boolean field with values `1` (checked) and `0` (unchecked).

---

## 5. Main Functions:

1. **`Create`**:
   - Initializes the frame and configures grid settings.
   - Sets read-only fields, hidden fields, and custom editors.

2. **`SetKeyEditing`**:
   - Disables editing for the `channelCd` field.

3. **`m_IsPortalActivated`**:
   - Checks if the portal channel is activated by verifying the `channelCd` and `checked` fields.

---

## 6. API Service Consumption:

No external API calls are explicitly defined in the code.

---

## 7. Conditional Fields (Form Logic):

- **Field**: `checked` (Checkbox).
- **Condition**: Always visible and editable.

---

## 8. Dependencies:

### External Libraries:
- **cxGrid**: Used for grid display and management.
- **cxEditRepository**: Provides custom editors for grid fields.
- **SOAPHTTPClient**: Enables SOAP-based service integration.

### Custom Components:
- **TFRAMEBaseGridEditSOA**: Base frame providing grid-editing functionalities.

---

## 9. Fields and Validations Listing:

1. **channelCd**:
   - Type: String.
   - Read-only: Yes.
   - Validation: Not explicitly defined in the code.

2. **chDescrip**:
   - Type: String.
   - Read-only: Yes.
   - Validation: Not explicitly defined in the code.

3. **checked**:
   - Type: Boolean (Checkbox).
   - Default: Unchecked (`0`).
   - Validation: Not explicitly defined in the code.

---

## 10. Examples and Diagrams:

### Flowchart:
Not applicable.

### Sequence Diagram:
Not applicable.

### Code Snippets:
```pascal
// Example: Checking if the portal is activated
if FRAMEcustBusinessChannel.m_IsPortalActivated then
  ShowMessage('Portal is activated')
else
  ShowMessage('Portal is not activated');
```

### Screenshots:
The DFM file represents a grid. Below is the HTML representation:

```html
<table style="font-family: Verdana; border: 1px solid black; width: 100%;">
  <thead>
    <tr>
      <th>Checked</th>
      <th>Channel Code</th>
      <th>Description</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td><input type="checkbox" /></td>
      <td>001</td>
      <td>Channel A</td>
    </tr>
    <tr>
      <td><input type="checkbox" /></td>
      <td>002</td>
      <td>Channel B</td>
    </tr>
  </tbody>
</table>
```

---

## 11. Important Comments in the Code:

- **Grid Settings**: Configures read-only fields, hidden fields, and custom editors.
- **Portal Activation Check**: Implements logic to verify if the portal channel is activated.

---

## 12. Conclusion:

The `FRcustBusinessChannel` frame provides a structured and configurable grid for managing customer business channels. It includes features like read-only fields, hidden fields, and custom editors. However, it lacks error handling and filtering options, which could enhance its usability.

---

## 13. Short Summary:

The `FRcustBusinessChannel` frame manages customer business channels in a grid format, offering features like read-only fields, hidden fields, and portal activation checks. It is part of a larger system for customer data management.#### **FRcustBusinessChannel.pas**

```
unit FRcustBusinessChannel;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, ImgList, ActnList, ExtCtrls, Rio, SOAPHTTPClient,
  DBClient, StdCtrls, Buttons, cxGridLevel, cxGridCustomTableView,
  cxGridTableView, cxGridDBTableView, cxClasses, cxControls,
  cxGridCustomView, cxGrid, sFrameAdapter, kneFRGridManager, sBitBtn,
  sPanel;

type
  TFRAMEcustBusinessChannel = class(TFRAMEBaseGridEditSOA)
    cxEDTchecked: TcxEditRepositoryCheckBoxItem;
  private
   { Private declarations }

   public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure SetKeyEditing(const EditKey: Boolean);  override;

    function m_IsPortalActivated: Boolean;

  end;

var
  FRAMEcustBusinessChannel: TFRAMEcustBusinessChannel;

implementation

uses
  kneTypes;

const
  mc_GRID_FIELDS = 'channelCd;chDescrip';

{$R *.dfm}

{ TFRAMEcustBusinessChannel }

constructor TFRAMEcustBusinessChannel.Create(AOwner: TComponent);
begin
  inherited;
  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'customerCode=custCd';
  DataPacketName := 'CustChannel';
  PropertyName := 'channels';
  FrameType := frtDetail;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := False;
  AvailableActions := '';

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin

    // Campos Read-Only ........................................................
    DefineReadOnlyFields(mc_GRID_FIELDS);
    // Campos Hidden ...........................................................
    DefineHiddenFields('HIDE_ALL_FIELDS');
    // Ordem Campos ............................................................
    DefineOrderFields('checked;' +  mc_GRID_FIELDS);
    // Key Fields ..............................................................
//    KeyFields:= '';
    // Custom Editors ..........................................................
    AddCustomField('checked','cxEDTchecked');
  end; //with


end;

procedure TFRAMEcustBusinessChannel.SetKeyEditing(const EditKey: Boolean);
begin
  inherited;
  // N�o Permite a edi��o do campo channelCd
  cxDBVtable.GetColumnByFieldName('channelCd').Options.Editing := False;
end;

// [2021/01/20, #24241]
function TFRAMEcustBusinessChannel.m_IsPortalActivated: Boolean;
begin
  Result := False;

  if CDStable.Locate('channelCd', VarArrayOf(['P']), [loCaseInsensitive])then
    Result := (CDStable.fieldByName('checked').Value = 1);

end;


end.
```

#### **FRcustBusinessChannel.dfm**

```
inherited FRAMEcustBusinessChannel: TFRAMEcustBusinessChannel
  Font.Name = 'Verdana'
  inherited PNLfooter: TsPanel
    Visible = False
  end
  inherited cxSTLR: TcxStyleRepository
    inherited cxSTLReadOnly: TcxStyle
      Font.Name = 'Verdana'
    end
    inherited cxSTLDefault: TcxStyle
      Font.Name = 'Verdana'
    end
    inherited cxSTLInactive: TcxStyle
      Font.Name = 'Verdana'
    end
    inherited cxSTLgroupBox: TcxStyle
      Font.Name = 'Verdana'
    end
    inherited cxSTLheader: TcxStyle
      Font.Name = 'Verdana'
    end
    inherited cxSTLselection: TcxStyle
      Font.Name = 'Verdana'
    end
  end
  inherited cxEDTR: TcxEditRepository
    object cxEDTchecked: TcxEditRepositoryCheckBoxItem
      Properties.ValueChecked = '1'
      Properties.ValueUnchecked = '0'
    end
  end
end
```
<!-- tabs:end -->

