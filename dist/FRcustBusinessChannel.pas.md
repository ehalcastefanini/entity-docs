<!-- tabs:start -->

#### **Documentation**

# Documentação do Código: Unidade `FRcustBusinessChannel`

## 1. Visão Geral:

### Objetivo Principal:
O objetivo principal deste código é gerenciar e exibir informações relacionadas aos canais de negócios de clientes em uma interface de grade (grid). Ele permite a visualização, configuração e manipulação de dados de canais de negócios associados a clientes, com funcionalidades específicas como campos somente leitura, campos ocultos e validação de dados.

### Tecnologias Utilizadas:
- **Delphi**: Linguagem de programação utilizada para desenvolver a aplicação.
- **Componentes VCL**: Incluindo `TcxGrid`, `TcxEditRepositoryCheckBoxItem`, e outros componentes visuais para a interface.
- **SOAP**: Para comunicação com serviços externos.
- **Banco de Dados**: Manipulação de dados através de `DBClient` e `CDStable`.

### Tipo de Interface:
- **Exibição em Grade (Grid Display)**:
  - **Colunas da Grade**:
    - `channelCd` (Código do Canal) - Tipo: String.
    - `chDescrip` (Descrição do Canal) - Tipo: String.
    - `checked` (Marcado) - Tipo: Checkbox.
  - **Ações da Grade**:
    - Configuração de campos somente leitura.
    - Ocultação de campos.
    - Definição de ordem de exibição das colunas.
    - Validação de edição de campos.

---

## 2. Descrição da Funcionalidade:

### Ações Disponíveis:
- Exibir dados de canais de negócios em uma grade.
- Configurar campos como somente leitura ou ocultos.
- Validar se o canal está ativado no portal.
- Impedir edição de campos específicos.

### Componentes Principais:
- **`TFRAMEcustBusinessChannel`**: Classe principal que herda de `TFRAMEBaseGridEditSOA` e implementa a lógica de exibição e manipulação da grade.
- **`cxEDTchecked`**: Item de repositório de edição para gerenciar checkboxes na grade.

### Pseudo-código de Ações e Eventos:
- **Construtor (`Create`)**:
  ```pseudo
  ao criar o frame:
    configurar campos somente leitura
    ocultar campos
    definir ordem dos campos
    adicionar editor customizado para o campo "checked"
  ```
- **Evento `SetKeyEditing`**:
  ```pseudo
  se edição de chave for ativada:
    desativar edição do campo "channelCd"
  ```
- **Função `m_IsPortalActivated`**:
  ```pseudo
  se o canal "P" for encontrado na tabela:
    verificar se o campo "checked" está marcado
  ```

---

## 3. Lógica Operacional:

### Fluxo de Execução:
1. **Inicialização**:
   - O construtor `Create` é chamado ao criar o frame.
   - Configurações da grade são aplicadas, como campos somente leitura, ocultos e ordem de exibição.
2. **Interações do Usuário**:
   - O usuário pode visualizar os dados na grade.
   - A edição do campo `channelCd` é desativada.
3. **Validação**:
   - A função `m_IsPortalActivated` verifica se o canal está ativado no portal.

### Dados Necessários:
- Dados de canais de negócios, incluindo `channelCd`, `chDescrip` e `checked`.

---

## 4. Regras de Negócio:

### Ações e Pré-condições:
- **Edição de Campos**:
  - O campo `channelCd` não pode ser editado.
- **Validação de Portal**:
  - O canal deve ser identificado como "P" e marcado como ativo (`checked = 1`).

### Filtros Disponíveis:
- Não há filtros explícitos definidos no código.

### Mensagens de Erro:
- Não há mensagens de erro explícitas definidas no código.

### Valores Padrão dos Campos:
- `checked`: Valor padrão é `0` (não marcado).

### Validações e Condições:
- O campo `checked` utiliza valores `1` (marcado) e `0` (não marcado).

---

## 5. Funções Principais:

### Funções:
1. **`Create`**:
   - Configura a grade com campos somente leitura, ocultos e ordem de exibição.
2. **`SetKeyEditing`**:
   - Desativa a edição do campo `channelCd`.
3. **`m_IsPortalActivated`**:
   - Verifica se o canal "P" está ativado no portal.

---

## 6. Consumo de Serviços API:

- **Nenhuma chamada a serviços externos é explicitamente definida no código.**

---

## 7. Campos Condicionais (Lógica de Formulário):

- Não há campos condicionais definidos no código.

---

## 8. Dependências:

### Bibliotecas Externas:
- **SOAP**: Para comunicação com serviços externos.
- **VCL**: Componentes visuais e de manipulação de dados.

### Componentes Customizados:
- **`TFRAMEBaseGridEditSOA`**: Classe base para frames de edição em grade.
- **`TcxEditRepositoryCheckBoxItem`**: Repositório de edição para checkboxes.

---

## 9. Listagem de Campos e Validações:

### Campos:
1. **`channelCd`**:
   - Tipo: String.
   - Somente leitura.
2. **`chDescrip`**:
   - Tipo: String.
   - Somente leitura.
3. **`checked`**:
   - Tipo: Checkbox.
   - Valores: `1` (marcado), `0` (não marcado).

### Mapeamento:
- `channelCd` → Coluna no banco de dados.
- `chDescrip` → Coluna no banco de dados.
- `checked` → Coluna no banco de dados.

---

## 10. Exemplos e Diagramas:

### Fluxograma:
**Não aplicável.**

### Diagrama de Sequência:
**Não aplicável.**

### Código HTML Representando a Grade:
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
      <td><input type="checkbox" disabled></td>
      <td>001</td>
      <td>Canal de Vendas</td>
    </tr>
    <tr>
      <td><input type="checkbox" disabled></td>
      <td>002</td>
      <td>Canal de Marketing</td>
    </tr>
  </tbody>
</table>
```

---

## 11. Comentários Importantes no Código:

- **Configuração de Propriedades da Grade**:
  ```delphi
  // Configurar visibilidade de painel de ações e ações disponíveis
  ShowActionPanel := False;
  AvailableActions := '';
  ```

- **Validação de Portal**:
  ```delphi
  // Verifica se o canal "P" está ativado no portal
  if CDStable.Locate('channelCd', VarArrayOf(['P']), [loCaseInsensitive]) then
  ```

---

## 12. Conclusão:

O código implementa uma interface de grade para gerenciar canais de negócios de clientes. Ele é bem estruturado para exibir e configurar dados, mas não possui mensagens de erro ou filtros avançados. Sua força está na flexibilidade de configuração da grade, enquanto sua limitação é a ausência de validações mais robustas.

---

## 13. Resumo Curto:

O código gerencia uma grade de canais de negócios de clientes, permitindo exibição e validação de dados. Ele desativa a edição de campos específicos e verifica a ativação de canais no portal. É parte de um sistema maior para gerenciamento de dados de clientes.#### **FRcustBusinessChannel.pas**

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

